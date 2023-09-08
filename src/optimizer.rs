use std::{collections::VecDeque, rc::Rc, cell::RefCell};

use crate::{cfg::{Graph, Terminator, Constant, InstructionKind, Operand, Node, Instruction}, memory::MemRef};

pub fn optimize(cfg: &mut Graph) {
    let mut first = true;
    loop {
        let mut changed = repeat(eliminate_unneeded_phi, cfg);
        changed = changed || constant_folding(cfg);
        changed = changed || repeat(remove_dead_instructions, cfg);
        if first { changed = changed || common_subexpression_elimination(cfg); }
        // warning: dominator tree breaks here
        changed = changed || repeat(optimize_const_goto, cfg);
        changed = changed || repeat(remove_dead_code, cfg);

        if !changed { break; }
        first = false;
    }
}

fn repeat(f: impl Fn(&mut Graph) -> bool, cfg: &mut Graph) -> bool {
    let mut changed = false;
    while f(cfg) { changed = true; }
    changed
}

fn eliminate_unneeded_phi(cfg: &mut Graph) -> bool {
    let mut changed = false;
    for node in cfg.nodes().iter() {
        let instructions = &mut node.get_mut().instructions;
        let mut i = 0;
        while i < instructions.len() {
            let instruction = &instructions[i];
            let mut instruction_ = instruction.get_mut();
            let first;
            if let InstructionKind::Phi(operands, _) = &instruction_.kind {
                if operands.is_empty() {
                    i += 1;
                    continue;
                }

                let mut all_same = true;
                first = operands[0].clone();
                for operand in operands.iter().skip(1) {
                    if *operand != first {
                        all_same = false;
                        break;
                    }
                }
                if !all_same {
                    i += 1;
                    continue;
                }
            }
            else {
                i += 1;
                continue;
            }

            for used_at in &instruction_.used_at {
                used_at.get_mut().replace_operand(Operand::Instruction(instruction.clone()), first.clone());
                used_at.get_mut().try_calculate_constant();
            }
            instruction_.used_at.clear();
            drop(instruction_);
            instructions.remove(i);
            changed = true;
        }
    }
    changed
}

fn constant_folding(cfg: &mut Graph) -> bool {
    let mut changed = false;
    let mut queue = VecDeque::new();
    for instruction in cfg.instructions().iter() {
        queue.push_back(instruction.clone());
    }
    while !queue.is_empty() {
        let instruction = queue.pop_front().unwrap();
        let constant = instruction.get().constant;
        if let Constant::Integer(constant) = constant {
            changed = true;
            for used_at in &instruction.get().used_at {
                used_at.get_mut().replace_operand(Operand::Instruction(instruction.clone()), Operand::Constant(constant));
                used_at.get_mut().try_calculate_constant();
                queue.push_back(used_at.clone());
            }
            instruction.get_mut().used_at.clear();
        }
    }
    changed
}

fn remove_dead_instructions(cfg: &mut Graph) -> bool {
    let mut changed = false;
    for node in cfg.nodes().iter() {
        let instructions = &mut node.get_mut().instructions;
        let mut i = 0;
        while i < instructions.len() {
            let instruction = &instructions[i];
            if instruction.get().used_at.is_empty() && !instruction.get().kind.is_always_alive() {
                instructions.remove(i);
                changed = true;
            }
            else {
                i += 1;
            }
        }
    }
    changed
}

fn common_subexpression_elimination(cfg: &mut Graph) -> bool {
    fn travel_dominator_tree(current: MemRef<Node>, op_list: Rc<RefCell<OpList>>) -> bool {
        let mut changed = false;
        {
            let instructions = &mut current.get_mut().instructions;
            let mut i = 0;
            while i < instructions.len() {
                let instruction = &instructions[i];
                let similar = op_list.borrow().try_find_similar(instruction);
                if let Some(similar) = similar {
                    for used_at in &instruction.get().used_at {
                        similar.get_mut().used_at.push(used_at.clone());
                        used_at.get_mut().replace_operand(Operand::Instruction(instruction.clone()), Operand::Instruction(similar.clone()));
                        used_at.get_mut().try_calculate_constant();
                    }
                    instruction.get_mut().used_at.clear();
                    instructions.remove(i);
                    changed = true;
                }
                else {
                    op_list.borrow_mut().insert(instruction.clone());
                    i += 1;
                }
            }
        }

        for node in &current.get().dominating {
            if travel_dominator_tree(node.clone(), OpList::new(Some(op_list.clone()))) {
                changed = true;
            }
        }

        changed
    }

    struct OpList {
        parent: Option<Rc<RefCell<OpList>>>,
        add: Vec<MemRef<Instruction>>,
        sub: Vec<MemRef<Instruction>>,
        mul: Vec<MemRef<Instruction>>,
        div: Vec<MemRef<Instruction>>,
        mod_: Vec<MemRef<Instruction>>,
        neg: Vec<MemRef<Instruction>>,
        cmp: Vec<MemRef<Instruction>>,
        phi: Vec<MemRef<Instruction>>,
        array_load: Vec<MemRef<Instruction>>,
    }
    impl OpList {
        fn new(parent: Option<Rc<RefCell<Self>>>) -> Rc<RefCell<Self>> {
            Rc::new(RefCell::new(Self {
                parent,
                add: vec![],
                sub: vec![],
                mul: vec![],
                div: vec![],
                mod_: vec![],
                neg: vec![],
                cmp: vec![],
                phi: vec![],
                array_load: vec![],
            }))
        }

        fn try_find_similar(&self, instruction: &MemRef<Instruction>) -> Option<MemRef<Instruction>> {
            macro find($name:ident) {{
                for similar in self.$name.iter().rev() {
                    if similar.get().kind == instruction.get().kind {
                        return Some(similar.clone());
                    }
                }
                if let Some(parent) = &self.parent {
                    return parent.borrow().try_find_similar(instruction);
                }
            }}
            match &instruction.get().kind {
                InstructionKind::Add(_, _) => find!(add),
                InstructionKind::Sub(_, _) => find!(sub),
                InstructionKind::Mul(_, _) => find!(mul),
                InstructionKind::Div(_, _) => find!(div),
                InstructionKind::Mod(_, _) => find!(mod_),
                InstructionKind::Neg(_) => find!(neg),
                InstructionKind::Cmp(_, _, _) => find!(cmp),
                InstructionKind::Phi(_, _) => find!(phi),
                InstructionKind::ArrayLoad(_, _, variable) => {
                    for similar in self.array_load.iter().rev() {
                        if let InstructionKind::ArrayStore(_, _, _, into) = &similar.get().kind {
                            if variable == into {
                                return None;
                            }
                        }
                        if similar.get().kind == instruction.get().kind {
                            return Some(similar.clone());
                        }
                    }
                    if let Some(parent) = &self.parent {
                        return parent.borrow().try_find_similar(instruction);
                    }
                },
                _ => {},
            }
            None
        }

        fn insert(&mut self, instruction: MemRef<Instruction>) {
            let kind = instruction.get().kind.clone(); // rust madness
            match kind {
                InstructionKind::Add(_, _) => self.add.push(instruction),
                InstructionKind::Sub(_, _) => self.sub.push(instruction),
                InstructionKind::Mul(_, _) => self.mul.push(instruction),
                InstructionKind::Div(_, _) => self.div.push(instruction),
                InstructionKind::Mod(_, _) => self.mod_.push(instruction),
                InstructionKind::Neg(_) => self.neg.push(instruction),
                InstructionKind::Cmp(_, _, _) => self.cmp.push(instruction),
                InstructionKind::Phi(_, _) => self.phi.push(instruction),
                InstructionKind::ArrayLoad(_, _, _) => self.array_load.push(instruction),
                InstructionKind::ArrayStore(_, _, _, _) => self.array_load.push(instruction),
                _ => {},
            }
        }
    }

    travel_dominator_tree(cfg.entry(), OpList::new(None))
}

fn optimize_const_goto(cfg: &mut Graph) -> bool {
    let mut changed = false;
    'outer: for node in cfg.nodes().iter() {
        let node_ = node.get();
        match &node_.terminator {
            Terminator::Goto(target) => {
                let mut phis = vec![];
                for instruction in &node_.instructions {
                    if let InstructionKind::Phi(_, _) = instruction.get().kind {
                        phis.push(instruction.clone());
                    }
                    else {
                        continue 'outer;
                    }
                }
                // TODO
                if !phis.is_empty() {
                    continue;
                }

                let target = target.clone();
                let id = node_.id;
                let predecessors = node_.predecessors.clone();
                drop(node_);
                for predecessor in predecessors {
                    _remove_predecessor(&predecessor, &node);
                    let mut predecessor_ = predecessor.get_mut();
                    match &mut predecessor_.terminator {
                        Terminator::Goto(pred_target) => {
                            *pred_target = target.clone();
                            drop(predecessor_);
                            link(&target, &predecessor, &node);
                            changed = true;
                        },
                        Terminator::BranchIf {
                            condition: _,
                            then,
                            els,
                        } => {
                            let then_id = then.get().id;
                            let els_id = els.get().id;
                            if then_id == id {
                                *then = target.clone();
                                changed = true;
                            }
                            if els_id == id {
                                *els = target.clone();
                                changed = true;
                            }
                            drop(predecessor_);
                            if then_id == id {
                                link(&target, &predecessor, &node);
                            }
                            if els_id == id {
                                link(&target, &predecessor, &node);
                            }
                        },
                        Terminator::Return => {},
                    }
                }

                fn link(new_target: &MemRef<Node>, node: &MemRef<Node>, removed: &MemRef<Node>) {
                    new_target.get_mut().predecessors.push(node.clone());
                    for instruction in &new_target.get().instructions {
                        if let InstructionKind::Phi(operands, name) = &mut instruction.get_mut().kind {
                            operands.push(removed.get().lookup_variable(name));
                        }
                    }
                }
            },
            Terminator::BranchIf {
                condition,
                then,
                els,
            } => {
                let condition = condition.get().constant;
                if let Constant::Boolean(value) = condition {
                    let then = then.clone();
                    let els = els.clone();
                    drop(node_);
                    if value {
                        _remove_predecessor(&node, &els);
                        _remove_cmp_instruction(&node);
                        node.get_mut().terminator = Terminator::Goto(then);
                    }
                    else {
                        _remove_predecessor(&node, &then);
                        _remove_cmp_instruction(&node);
                        node.get_mut().terminator = Terminator::Goto(els);
                    }
                    changed = true;
                }
                else if then.get().id == els.get().id {
                    let first_index = then.get().predecessors.iter().position(|p| p.get().id == node.get().id).unwrap();
                    let second_index = then.get().predecessors.iter().skip(first_index + 1).position(|p| p.get().id == node.get().id).unwrap();
                    for instruction in &node_.instructions {
                        if let InstructionKind::Phi(operands, _) = &instruction.get().kind {
                            if operands[first_index] != operands[second_index] {
                                continue 'outer;
                            }
                        }
                    }

                    node.get_mut().terminator = Terminator::Goto(then.clone());
                    _remove_predecessor(&node, &els);
                    _remove_cmp_instruction(&node);
                }
            },
            Terminator::Return => {},
        }
    }
    changed
}

fn remove_dead_code(cfg: &mut Graph) -> bool {
    let mut dead = vec![];
    for node in cfg.nodes().iter() {
        if node.get().predecessors.is_empty() {
            dead.push(node);
        }
    }
    let changed = !dead.is_empty();
    for node in dead {
        _unlink_node(&node);
        cfg.remove_node(node);
    }
    changed
}


fn _remove_cmp_instruction(node: &MemRef<Node>) {
    let last = node.get_mut().instructions.len() - 1; // rust :skull:
    let last = node.get_mut().instructions.remove(last);
    let last = last.get();
    if let InstructionKind::Cmp(_, _, _) = last.kind {
    }
    else {
        panic!("internal compiler error");
    }
}

fn _remove_predecessor(predecessor: &MemRef<Node>, from: &MemRef<Node>) {
    let mut from = from.get_mut();
    let predecessors = &mut from.predecessors;
    let index = predecessors.iter().position(|p| p.get().id == predecessor.get().id).unwrap();
    predecessors.remove(index);

    let instructions = &mut from.instructions;
    let mut i = 0;
    while i < instructions.len() {
        let instruction = &instructions[i];
        if let InstructionKind::Phi(operands, _) = &mut instruction.get_mut().kind {
            operands.remove(index);
        }
        i += 1;
    }
}

fn _unlink_node(node: &MemRef<Node>) {
    match &node.get().terminator {
        Terminator::Goto(to) => {
            _remove_predecessor(&node, &to);
        },
        Terminator::BranchIf {
            condition: _,
            then,
            els,
        } => {
            _remove_cmp_instruction(&node);
            _remove_predecessor(&node, &then);
            _remove_predecessor(&node, &els);
        },
        Terminator::Return => {},
    }
}
