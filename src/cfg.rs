use std::{rc::Rc, cell::RefCell, fmt::{Debug, self}, collections::HashMap};

use crate::{ast, memory::{MemPool, MemRef}, types::Scope};

/// Control flow graph for storing one program. Contains both nodes and
/// instructions in memory pools.
pub struct Graph {
    nodes: MemPool<Node>,
    entry: Rc<RefCell<Node>>,
    node_id: usize,
    instructions: MemPool<Instruction>,
    instruction_id: usize,
}

impl Graph {
    /// Creates a new empty control flow graph.
    pub fn new() -> Self {
        let entry_node = Rc::new(RefCell::new(Node {
            id: 0,
            instructions: vec![],
            predecessors: vec![],
            terminator: Terminator::Return,
            latest_variable_instance: HashMap::new(),

            dominator: None,
            dominating: vec![],
        }));
        Self {
            nodes: MemPool::new(),
            entry: entry_node,
            node_id: 1,
            instructions: MemPool::new(),
            instruction_id: 1,
        }
    }

    pub fn new_node(&mut self) -> MemRef<Node> {
        self.node_id += 1;
        self.nodes.add(Node {
            id: self.node_id - 1,
            instructions: vec![],
            predecessors: vec![],
            terminator: Terminator::Return,
            latest_variable_instance: HashMap::new(),

            dominator: None,
            dominating: vec![],
        })
    }

    pub fn new_instruction(&mut self, instruction: InstructionKind) -> MemRef<Instruction> {
        self.instruction_id += 1;
        self.instructions.add(Instruction {
            id: self.instruction_id,
            kind: instruction,
            constant: Constant::None,
            used_at: vec![],
        })
    }

    pub fn entry(&self) -> MemRef<Node> {
        Rc::downgrade(&self.entry).into()
    }

    pub fn nodes(&mut self) -> &mut MemPool<Node> {
        &mut self.nodes
    }

    pub fn instructions(&mut self) -> &mut MemPool<Instruction> {
        &mut self.instructions
    }

    pub fn remove_node(&mut self, node: MemRef<Node>) {
        let id = node.get().id;
        let index = self.nodes.iter().position(|n| n.get().id == id).unwrap();
        let node = self.nodes.get(index);
        if !node.get().predecessors.is_empty() {
            return;
        }
        self.nodes.remove(index);
    }

    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("digraph {\n");
        for node in self.nodes.iter() {
            let node = node.get();
            let label = node.instructions.iter().map(|i| match i.get().kind {
                InstructionKind::Add(ref a, ref b) => format!("%{} = add {:?}, {:?}", i.get().id, a, b),
                InstructionKind::Sub(ref a, ref b) => format!("%{} = sub {:?}, {:?}", i.get().id, a, b),
                InstructionKind::Mul(ref a, ref b) => format!("%{} = mul {:?}, {:?}", i.get().id, a, b),
                InstructionKind::Div(ref a, ref b) => format!("%{} = div {:?}, {:?}", i.get().id, a, b),
                InstructionKind::Mod(ref a, ref b) => format!("%{} = mod {:?}, {:?}", i.get().id, a, b),
                InstructionKind::Neg(ref a) => format!("%{} = neg {:?}", i.get().id, a),

                InstructionKind::Cmp(ref a, ref b, operator) => format!("cmp {:?}, {:?} ({})", a, b, operator.str_repr()),

                InstructionKind::Phi(ref operands, ref name) => format!("%{} = phi {:?} ({})", i.get().id, operands, name),

                InstructionKind::ArrayLoad(ref base, ref index, ref uses) => format!("%{} = load {}[{:?}] ({})", i.get().id, -base, index, uses),
                InstructionKind::ArrayStore(ref base, ref index, ref value, ref kills) => format!("store {}[{:?}], {:?} ({})", -base, index, value, kills),

                InstructionKind::Read => format!("%{} = read", i.get().id),
                InstructionKind::Write(ref value) => format!("write {:?}", value),

                InstructionKind::Ret => "ret".to_string(),
                InstructionKind::__Start => "[[START]]".to_string(),
                InstructionKind::__End => "[[END]]".to_string(),
            } + "\\l").collect::<Vec<_>>().join("");
            dot.push_str(&format!("  {} [label=\"{}\", shape=box];\n", node.id, label));
            match &node.terminator {
                Terminator::Return => {}
                Terminator::Goto(next) => {
                    dot.push_str(&format!("  {} -> {};\n", node.id, next.get().id));
                }
                Terminator::BranchIf { then, els, .. } => {
                    dot.push_str(&format!("  {} -> {} [label=\"then\", color=green];\n", node.id, then.get().id));
                    dot.push_str(&format!("  {} -> {} [label=\"else\", color=red];\n", node.id, els.get().id));
                }
            }
        }
        dot.push_str("}\n");
        dot
    }
}

/// A node in the control flow graph, containing multiple instructions. The
/// node is terminated by a [`Terminator`].
pub struct Node {
    pub id: usize,
    pub instructions: Vec<MemRef<Instruction>>,
    pub predecessors: Vec<MemRef<Node>>,
    pub terminator: Terminator,
    pub latest_variable_instance: HashMap<String, Operand>,

    pub dominator: Option<MemRef<Node>>,
    pub dominating: Vec<MemRef<Node>>,
}

impl Node {
    pub fn lookup_variable(&self, name: &str) -> Operand {
        self.latest_variable_instance
            .get(name)
            .cloned()
            .unwrap_or(Operand::Constant(0))
    }
}

/// The end of a node in the control flow graph. A node is terminated by one of
/// these.
pub enum Terminator {
    Return,
    Goto(MemRef<Node>),
    BranchIf {
        condition: MemRef<Instruction>,
        then: MemRef<Node>,
        els: MemRef<Node>,
    },
}

/// TODO: document
#[derive(Debug, Clone)]
pub struct Condition {
    pub instruction: MemRef<Instruction>,
    pub operator: ConditionOperator,
}

/// Operator for comparing two values. Used in a conditional terminator.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ConditionOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl ConditionOperator {
    pub fn str_repr(&self) -> &'static str {
        match self {
            ConditionOperator::Equal => "=",
            ConditionOperator::NotEqual => "#",
            ConditionOperator::LessThan => "<",
            ConditionOperator::LessThanOrEqual => "<=",
            ConditionOperator::GreaterThan => ">",
            ConditionOperator::GreaterThanOrEqual => ">=",
        }
    }

    pub fn eval(&self, a: i32, b: i32) -> bool {
        match self {
            ConditionOperator::Equal => a == b,
            ConditionOperator::NotEqual => a != b,
            ConditionOperator::LessThan => a < b,
            ConditionOperator::LessThanOrEqual => a <= b,
            ConditionOperator::GreaterThan => a > b,
            ConditionOperator::GreaterThanOrEqual => a >= b,
        }
    }
}

impl From<ast::ConditionOperator> for ConditionOperator {
    fn from(op: ast::ConditionOperator) -> Self {
        match op {
            ast::ConditionOperator::Equal => ConditionOperator::Equal,
            ast::ConditionOperator::NotEqual => ConditionOperator::NotEqual,
            ast::ConditionOperator::LessThan => ConditionOperator::LessThan,
            ast::ConditionOperator::LessThanOrEqual => ConditionOperator::LessThanOrEqual,
            ast::ConditionOperator::GreaterThan => ConditionOperator::GreaterThan,
            ast::ConditionOperator::GreaterThanOrEqual => ConditionOperator::GreaterThanOrEqual,
        }
    }
}

/// TODO: document
#[derive(Debug, Clone)]
pub struct Instruction {
    pub id: usize,
    pub kind: InstructionKind,
    pub constant: Constant,
    pub used_at: Vec<MemRef<Instruction>>,
}

impl Instruction {
    pub fn for_each_operand(&mut self, f: impl Fn(&mut Operand)) {
        match &mut self.kind {
            InstructionKind::Add(a, b) => { f(a); f(b); },
            InstructionKind::Sub(a, b) => { f(a); f(b); },
            InstructionKind::Mul(a, b) => { f(a); f(b); },
            InstructionKind::Div(a, b) => { f(a); f(b); },
            InstructionKind::Mod(a, b) => { f(a); f(b); },
            InstructionKind::Neg(a) => { f(a); },
            InstructionKind::Cmp(a, b, _) => { f(a); f(b); },
            InstructionKind::Phi(operands, _) => { for op in operands { f(op); } },
            InstructionKind::ArrayLoad(_, index, _) => { f(index); },
            InstructionKind::ArrayStore(_, index, value, _) => { f(index); f(value); },
            InstructionKind::Read => {},
            InstructionKind::Write(value) => { f(value); },
            InstructionKind::Ret => {},
            InstructionKind::__Start => {},
            InstructionKind::__End => {},
        }
    }

    pub fn replace_operand(&mut self, old: Operand, new: Operand) {
        self.for_each_operand(|op| {
            if *op == old {
                *op = new.clone();
            }
        });
    }

    pub fn try_calculate_constant(&mut self) {
        macro when_const($( $op:ident ),+) {
            $(
                let $op = match $op.get_const_value() {
                    Constant::Integer(i) => i,
                    _ => return,
                };
            )+
        }
        match &self.kind {
            InstructionKind::Add(a, b) => {
                when_const!(a, b);
                self.constant = Constant::Integer(a + b);
            },
            InstructionKind::Sub(a, b) => {
                when_const!(a, b);
                self.constant = Constant::Integer(a - b);
            },
            InstructionKind::Mul(a, b) => {
                when_const!(a, b);
                self.constant = Constant::Integer(a * b);
            },
            InstructionKind::Div(a, b) => {
                when_const!(a, b);
                self.constant = Constant::Integer(a / b);
            },
            InstructionKind::Mod(a, b) => {
                when_const!(a, b);
                self.constant = Constant::Integer(a % b);
            },
            InstructionKind::Neg(a) => {
                when_const!(a);
                self.constant = Constant::Integer(-a);
            },
            InstructionKind::Cmp(a, b, operator) => {
                when_const!(a, b);
                self.constant = Constant::Boolean(operator.eval(a, b));
            },
            _ => {},
        }
    }
}

/// The type of an instruction, possibly containing one or more [`Operand`]s.
#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    Add(Operand, Operand),
    Sub(Operand, Operand),
    Mul(Operand, Operand),
    Div(Operand, Operand),
    Mod(Operand, Operand),
    Neg(Operand),

    Cmp(Operand, Operand, ConditionOperator),

    Phi(Vec<Operand>, String),

    ArrayLoad(i32, Operand, /* uses */ String),
    ArrayStore(i32, Operand, Operand, /* kill */ String),

    Read,
    Write(Operand),

    Ret,
    __Start,
    __End,
}

impl InstructionKind {
    /// Checks if the instruction is always alive, i.e. does not require any
    /// other instructions to depend on it to be considered alive in dead code
    /// elimination.
    pub fn is_always_alive(&self) -> bool {
        matches!(
            self,
            | InstructionKind::Cmp(_, _, _)
            | InstructionKind::ArrayStore(_, _, _, _)
            | InstructionKind::Write(_)
            | InstructionKind::Ret
            | InstructionKind::__Start
            | InstructionKind::__End
        )
    }
}

/// An operand is a piece of input to an instruction. It can be a constant
/// value or reference another instruction.
#[derive(Clone, PartialEq)]
pub enum Operand {
    Constant(i32),
    Instruction(MemRef<Instruction>),
}

impl Operand {
    pub fn get_const_value(&self) -> Constant {
        match self {
            Operand::Constant(c) => Constant::Integer(*c),
            Operand::Instruction(i) => i.get().constant,
        }
    }
}

impl Debug for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Constant(c) => write!(f, "{}", c),
            Operand::Instruction(i) => write!(f, "%{}", i.get().id),
        }
    }
}

/// A constant value determined for an instruction. This value might be used in
/// constant folding and copy propagation.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Constant {
    None,
    Integer(i32),
    Boolean(bool),
}

/// Generates a control flow graph from an AST.
pub fn build_cfg(ast: ast::Program, scope: &Scope) -> Graph {
    let mut graph = Graph::new();
    let entry = graph.entry();
    let next = graph.new_node();
    entry.get_mut().terminator = Terminator::Goto(next.clone());
    next.get_mut().dominator = Some(entry.clone());
    next.get_mut().predecessors.push(entry);
    let mut builder = GraphBuilder {
        graph,
        current: next,
        scope,
    };

    builder.build_program(ast);

    builder.graph
}

struct GraphBuilder<'a> {
    graph: Graph,
    current: MemRef<Node>,
    scope: &'a Scope,
}

impl GraphBuilder<'_> {
    fn split(&mut self, from: MemRef<Node>) -> MemRef<Node> {
        let to = self.graph.new_node();
        from.get_mut().terminator = Terminator::Goto(to.clone());
        to.get_mut().latest_variable_instance = from.get().latest_variable_instance.clone();
        to.get_mut().predecessors.push(from.clone());
        to.get_mut().dominator = Some(from);
        to
    }

    fn branch(&mut self, from: MemRef<Node>, to: MemRef<Node>) {
        from.get_mut().terminator = Terminator::Goto(to.clone());
        to.get_mut().predecessors.push(from);
    }

    fn emit(&mut self, instruction: InstructionKind) -> MemRef<Instruction> {
        let instruction = self.graph.new_instruction(instruction);
        self.current.get_mut().instructions.push(instruction.clone());
        instruction.get_mut().try_calculate_constant();
        instruction.get_mut().for_each_operand(|op| {
            if let Operand::Instruction(i) = op {
                i.get_mut().used_at.push(instruction.clone());
            }
        });
        instruction
    }

    fn build_program(&mut self, program: ast::Program) {
        self.emit(InstructionKind::__Start);
        self.build_statements(program.statements);
        self.emit(InstructionKind::Ret);
        self.emit(InstructionKind::__End);
        self.reverse_dominator_tree();
    }

    fn build_statements(&mut self, statements: Vec<ast::Statement>) {
        for statement in statements {
            self.build_statement(statement);
        }
    }

    fn build_statement(&mut self, statement: ast::Statement) {
        match statement {
            ast::Statement::If {
                condition,
                then_statements,
                else_statements,
            } => {
                let condition = self.build_condition(condition);
                let start = self.current.clone();

                let then = self.split(start.clone());
                self.current = then.clone();
                self.build_statements(then_statements);
                let then_end = self.current.clone();

                let els = self.split(start.clone());
                self.current = els.clone();
                self.build_statements(else_statements);
                let els_end = self.current.clone();

                let end = self.graph.new_node();
                start.get_mut().terminator = Terminator::BranchIf {
                    condition,
                    then,
                    els,
                };
                end.get_mut().latest_variable_instance = then_end.get().latest_variable_instance.clone();
                self.branch(then_end.clone(), end.clone());
                self.branch(els_end.clone(), end.clone());
                end.get_mut().dominator = Some(start);
                self.current = end.clone();

                for variable in self.scope.items.keys() {
                    let then_var = then_end.get().lookup_variable(variable);
                    let els_var = els_end.get().lookup_variable(variable);
                    if then_var != els_var {
                        let phi = self.emit(InstructionKind::Phi(vec![
                            then_var,
                            els_var,
                        ], variable.clone()));
                        end.get_mut().latest_variable_instance.insert(variable.clone(), Operand::Instruction(phi));
                    }
                }
            },
            ast::Statement::While {
                condition,
                statements,
            } => {
                let cond = self.split(self.current.clone());
                self.current = cond.clone();
                for variable in self.scope.items.keys() {
                    let old = self.current.get().lookup_variable(variable);
                    let phi = self.emit(InstructionKind::Phi(vec![
                        old,
                    ], variable.clone()));
                    self.current.get_mut().latest_variable_instance.insert(variable.clone(), Operand::Instruction(phi));
                }
                let condition = self.build_condition(condition);

                let body = self.split(self.current.clone());
                self.current = body.clone();
                self.build_statements(statements);
                self.branch(self.current.clone(), cond.clone());

                let end = self.graph.new_node();
                cond.get_mut().terminator = Terminator::BranchIf {
                    condition,
                    then: body,
                    els: end.clone(),
                };
                end.get_mut().latest_variable_instance = cond.get().latest_variable_instance.clone();
                end.get_mut().dominator = Some(cond.clone());

                for variable in self.scope.items.keys() {
                    let phi = cond.get().lookup_variable(variable);
                    if let Operand::Instruction(p) = &phi {
                        let operand;
                        if let InstructionKind::Phi(operands, _) = &mut p.get_mut().kind {
                            let new = self.current.get().lookup_variable(variable);
                            if new != phi {
                                // variable changed inside loop, add to phi
                                if let Operand::Instruction(i) = &new {
                                    i.get_mut().used_at.push(p.clone());
                                }
                                operands.push(new);
                                continue;
                            }
                            operand = operands[0].clone();
                        }
                        else {
                            continue;
                        }
                        // has not changed, eliminate phi
                        end.get_mut().latest_variable_instance.insert(variable.clone(), operand.clone());
                        for used_at in p.get().used_at.clone() {
                            used_at.get_mut().replace_operand(phi.clone(), operand.clone());
                            used_at.get_mut().try_calculate_constant();
                        }
                        let mut cond = cond.get_mut();
                        let index = cond.instructions.iter().position(|i| i == p).unwrap();
                        cond.instructions.remove(index);
                    }
                }

                end.get_mut().predecessors.push(cond);
                self.current = end;
            },
            ast::Statement::Read(designator) => {
                let designator = self.build_designator(designator);
                let value = Operand::Instruction(self.emit(InstructionKind::Read));
                self.store(designator, value);
            },
            ast::Statement::Write(expression) => {
                let value = self.build_expression(expression);
                self.emit(InstructionKind::Write(value));
            },
            ast::Statement::Assignment {
                designator,
                expression,
            } => {
                let designator = self.build_designator(designator);
                let value = self.build_expression(expression);
                self.store(designator, value);
            },
        }
    }

    fn build_condition(&mut self, condition: ast::Condition) -> MemRef<Instruction> {
        let left = self.build_expression(condition.left);
        let right = self.build_expression(condition.right);
        let operator = ConditionOperator::from(condition.operator);
        self.emit(InstructionKind::Cmp(left, right, operator))
    }

    fn build_expression(&mut self, expression: ast::Expression) -> Operand {
        match expression {
            ast::Expression::Constant(c) => Operand::Constant(c),
            ast::Expression::Binary { box left, operator, box right } => {
                let left = self.build_expression(left);
                let right = self.build_expression(right);
                Operand::Instruction(self.emit(match operator {
                    ast::BinaryOperator::Plus => InstructionKind::Add,
                    ast::BinaryOperator::Minus => InstructionKind::Sub,
                    ast::BinaryOperator::Times => InstructionKind::Mul,
                    ast::BinaryOperator::Div => InstructionKind::Div,
                    ast::BinaryOperator::Mod => InstructionKind::Mod,
                }(left, right)))
            },
            ast::Expression::Unary { operator, box operand } => {
                match operator {
                    ast::UnaryOperator::Negate => {
                        let operand = self.build_expression(operand);
                        Operand::Instruction(self.emit(InstructionKind::Neg(operand)))
                    }
                }
            },
            ast::Expression::Designator(designator) => {
                let designator = self.build_designator(designator);
                self.load(designator)
            },
        }
    }

    fn build_designator(&mut self, designator: ast::Designator) -> Designator {
        match designator {
            ast::Designator::Variable(name) => Designator::Variable(name),
            ast::Designator::Array { box designator, box index, __id } => {
                let designator = self.build_designator(designator);
                let index = self.build_expression(index);
                let item_size = *self.scope.__designators.get(&__id).unwrap() as i32;
                match designator {
                    Designator::Variable(name) => {
                        let offset = self.scope.stack.iter().find(|(n, _, _)| n == &name).unwrap().2 as i32;
                        if let Operand::Constant(index) = index {
                            Designator::Array(offset, index * item_size, Operand::Constant(0), name)
                        }
                        else {
                            let index = self.emit(InstructionKind::Mul(index, Operand::Constant(item_size)));
                            Designator::Array(offset, 0, Operand::Instruction(index), name)
                        }
                    },
                    Designator::Array(base, constant_offset, old_index, variable) => {
                        if let Operand::Constant(index) = index {
                            Designator::Array(base, constant_offset + index * item_size, old_index, variable)
                        }
                        else {
                            let new_index = self.emit(InstructionKind::Mul(index, Operand::Constant(item_size)));
                            let index = self.emit(InstructionKind::Add(old_index, Operand::Instruction(new_index)));
                            Designator::Array(base, constant_offset, Operand::Instruction(index), variable)
                        }
                    },
                }
            },
        }
    }

    fn load(&mut self, designator: Designator) -> Operand {
        match designator {
            Designator::Variable(name) => {
                self.current.get().lookup_variable(&name)
            },
            Designator::Array(base, constant_offset, index, variable) => {
                Operand::Instruction(self.emit(InstructionKind::ArrayLoad(base - constant_offset, index, variable)))
            },
        }
    }

    fn store(&mut self, designator: Designator, value: Operand) {
        match designator {
            Designator::Variable(name) => {
                self.current.get_mut().latest_variable_instance.insert(name.clone(), value.clone());
            },
            Designator::Array(base, constant_offset, index, variable) => {
                self.emit(InstructionKind::ArrayStore(base - constant_offset, index, value, variable));
            },
        }
    }

    fn reverse_dominator_tree(&mut self) {
        for node in self.graph.nodes().iter() {
            if let Some(dominator) = node.get().dominator.clone() {
                dominator.get_mut().dominating.push(node.clone());
            }
        }
    }
}

enum Designator {
    Variable(String),
    Array(i32, i32, Operand, String),
}
