const std = @import("std");
const Buffer = std.ArrayList;
const Map = std.StringHashMap;

const TOKEN = u8;
const OPEN_TERM='[';
const CLOSE_TERM=']';
const IMPL_TERM=':';
const ARG_TERM='.';
const OPEN_GROUP='(';
const CLOSE_GROUP=')';
const VIEW_GROUP='/';
const BOTTOM='!';
const APPEND='+';
const ATOM:TOKEN = 0;

const Token = struct {
	tag: TOKEN,
	text: []const u8,
	pos: u64
};

pub fn tokenize(mem: *const std.mem.Allocator, text: []const u8) Buffer(Token) {
	var tokens = Buffer(Token).init(mem.*);
	var i:u64 = 0;
	while (i < text.len) {
		const c = text[i];
		switch (c){
			' ' => { },
			'\t' => { },
			'\n' => { },
			OPEN_TERM,
			CLOSE_TERM,
			IMPL_TERM,
			ARG_TERM,
			OPEN_GROUP,
			CLOSE_GROUP,
			VIEW_GROUP,
			BOTTOM,
			APPEND => {
				tokens.append(Token{
					.tag=c,
					.text=text[i..i+1],
					.pos = i
				}) catch unreachable;
			},
			else => {
				var k = i;
				outer: while (k < text.len){
					const kc = text[k];
					switch (kc){
						' ', '\n', '\t',
						OPEN_TERM,
						CLOSE_TERM,
						IMPL_TERM,
						ARG_TERM,
						OPEN_GROUP,
						CLOSE_GROUP,
						VIEW_GROUP,
						BOTTOM,
						APPEND => {
							break :outer;
						},
						else => { }
					}
					k += 1;
				}
				tokens.append(Token{
					.tag = ATOM,
					.text = text[i..k],
					.pos = i
				}) catch unreachable;
				i = k-1;
			}
		}
		i += 1;
	}
	return tokens;
}

const Instruction = struct {
	name: ?Token,
	comp: ?Program,

	pub fn eql(a: Instruction, b: Instruction) bool {
		if (a.name) |n| {
			if (b.name) |m| {
				if (n.tag != m.tag){
					return false;
				}
				if (std.mem.eql(u8, n.text, m.text)){
					return true;
				}
			}
			else {
				return false;
			}
		}
		else if (a.comp) |c| {
			if (b.comp) |k| {
				if (c.data.items.len != k.data.items.len){
					return false;
				}
				for (c.data.items, k.data.items) |left, right| {
					if (Instruction.eql(left, right) == false){
						return false;
					}
				}
				return true;
			}
			else {
				return false;
			}
		}
		return false;
	}
};

const Arg = union(enum) {
	named: Instruction,
	atom: Instruction
};

const Term = struct {
	bound_start: u64,
	bound_end: u64,
	args: Buffer(Arg),
	body: []Instruction
};

const Program = struct {
	mem: *const std.mem.Allocator,
	tmp: *const std.mem.Allocator,
	data: Buffer(Instruction),

	pub fn show(self: *const Program) void {
		for (self.data.items) |inst| {
			if (inst.name) |n| {
				std.debug.print("{s}", .{n.text});
				if (inst.comp) |_|{
					std.debug.print("/", .{});
				}
				else{
					std.debug.print(" ", .{});
				}
			}
			if (inst.comp) |c|{
				std.debug.print("(", .{});
				c.show();
				std.debug.print(") ", .{});
			}
		}
	}
};

const ParseError = error {
	ExpectedGroup,
	ExpectedArg,
	UnknownToken
};

pub fn parse(mem: *const std.mem.Allocator, tokens: []const Token, i: *u64) ParseError!Program {
	var program = Program{
		.mem = mem,
		.tmp = mem,
		.data = Buffer(Instruction).init(mem.*)
	};
	while (i.* < tokens.len){
		const token = tokens[i.*];
		var inst = Instruction{
			.name = null,
			.comp = null 
		};
		switch (token.tag){
			ARG_TERM,
			OPEN_TERM,
			CLOSE_TERM,
			IMPL_TERM,
			BOTTOM,
			APPEND,
			ATOM => {
				inst.name = token;
				if (i.*+1 == tokens.len){
					program.data.append(inst) catch unreachable;
					return program;
				}
				if (tokens[i.*+1].tag == VIEW_GROUP){
					i.* += 1;
					if (i.*+1 == tokens.len){
						return ParseError.ExpectedGroup;
					}
					if (tokens[i.*+1].tag != OPEN_GROUP){
						return ParseError.ExpectedGroup;
					}
					i.* += 2;
					inst.comp = try parse(mem, tokens, i);
				}
				program.data.append(inst) catch unreachable;
			},
			OPEN_GROUP => {
				i.* += 1;
				inst.comp = try parse(mem, tokens, i);
				program.data.append(inst) catch unreachable;
			},
			CLOSE_GROUP => {
				return program;
			},
			else => {
				return ParseError.UnknownToken;
			}
		}
		i.* += 1;
	}
	return program;
}

pub fn parse_term(mem: *const std.mem.Allocator, instructions: []Instruction, i: *u64) ?Term {
	if (instructions.len-i.* < 3){
		return null;
	}
	const save = i.*;
	i.* += 1;
	var term = Term{
		.bound_start = 0,
		.bound_end = 0,
		.args = Buffer(Arg).init(mem.*),
		.body = instructions
	};
	while (i.* < instructions.len){
		const inst = instructions[i.*];
		if (inst.name) |n| {
			if (n.tag == IMPL_TERM){
				break;
			}
			if (n.tag == ARG_TERM){
				i.* += 1;
				if (i.* == instructions.len){
					i.* = save;
					return null;
				}
				term.args.append(Arg{.named = instructions[i.*]}) catch unreachable;
			}
			else{
				term.args.append(Arg{.atom = instructions[i.*]}) catch unreachable;
			}
		}
		i.* += 1;
	}
	i.* += 1;
	const body = i.*;
	if (i.* >= instructions.len){
		return null;
	}
	while (i.* < instructions.len){
		const inst = instructions[i.*];
		if (inst.name) |n|{
			if (n.tag == CLOSE_TERM){
				term.body = instructions[body .. i.*];
				term.bound_start = save;
				term.bound_end = i.*;
				i.* += 1;
				return term;
			}
		}
		i.* += 1;
	}
	return null;
}

pub fn apply(program: *Program, i: u64, term: Term) bool {
	var k: u64 = i;
	var arg: u64 = 0;
	var argmap = Map(Instruction).init(program.tmp.*);
	if (term.args.items.len == 0){
		return false;
	}
	while (arg < term.args.items.len){
		if (k == program.data.items.len){
			return false;
		}
		if (k >= term.bound_start and k < term.bound_end){
			return false;
		}
		if (term.args.items[arg] == .named){
			if (term.args.items[arg].named.name) |name|{
				if (argmap.get(name.text)) |inst|{
					if (Instruction.eql(inst, program.data.items[k]) == false){
						return false;
					}
				}
				else{
					argmap.put(name.text, program.data.items[k]) catch unreachable;
				}
			}
			else{
				return false;
			}
		}
		else{
			if (Instruction.eql(program.data.items[k], term.args.items[arg].atom) == false){
				return false;
			}
		}
		k += 1;
		arg += 1;
	}
	var data = Buffer(Instruction).init(program.mem.*);
	data.appendSlice(program.data.items[0..i]) catch unreachable;
	for (term.body) |atom| {
		if (atom.name) |name| {
			if (argmap.get(name.text)) |alias| {
				data.append(alias) catch unreachable;
				continue;
			}
		}
		data.append(atom) catch unreachable;
	}
	data.appendSlice(program.data.items[k..program.data.items.len]) catch unreachable;
	program.data = data;
	return true;
}

pub fn eval_step(program: *Program) bool {
	var i: u64 = 0;
	var terms = Buffer(Term).init(program.mem.*);
	while (i < program.data.items.len) {
		const inst = program.data.items[i];
		if (inst.name) |n| {
			if (n.tag == OPEN_TERM){
				if (parse_term(program.mem, program.data.items, &i)) |term| {
					terms.append(term) catch unreachable;
				}
			}
		}
		i += 1;
	}
	i = 0;
	while (i < program.data.items.len){
		if (intrinsic_append(program, i)){
			return true;
		}
		if (intrinsic_bottom(program, i)){
			return true;
		}
		i += 1;
	}
	i = 0;
	while (i < program.data.items.len){
		var k: u64 = terms.items.len;
		while (k > 0){
			const term = terms.items[k-1];
			if (apply(program, i, term)){
				return true;
			}
			k -= 1;
		}
		i += 1;
	}
	return false;
}

pub fn append_step(program: *Program, stream: *Program) bool {
	if (stream.data.items.len == 0){
		return false;
	}
	program.data.append(stream.data.items[0]) catch unreachable;
	_ = stream.data.orderedRemove(0);
	while (eval_step(program)){
		std.debug.print(" => ", .{});
		program.show();
		std.debug.print("\n", .{});
	}
	return true;
}

pub fn intrinsic_append(program: *Program, i: u64) bool {
	if (i+2 >= program.data.items.len){
		return false;
	}
	if (program.data.items[i].comp) |_| {
		if (program.data.items[i+1].name) |name| {
			if (name.tag == APPEND){
				program.data.items[i].comp.?.data.append(program.data.items[i+2]) catch unreachable;
				_ = program.data.orderedRemove(i+1);
				_ = program.data.orderedRemove(i+1);
				return true;
			}
		}
	}
	return false;
}

pub fn intrinsic_bottom(program: *Program, i: u64) bool {
	if (i+1 >= program.data.items.len){
		return false;
	}
	if (program.data.items[i+1].name) |bot| {
		if (program.data.items[i].comp) |composition| {
			if (bot.tag == BOTTOM){
				var data = Buffer(Instruction).init(program.mem.*);
				data.appendSlice(program.data.items[0..i]) catch unreachable;
				data.appendSlice(composition.data.items) catch unreachable;
				data.appendSlice(program.data.items[i+2..program.data.items.len]) catch unreachable;
				program.data = data;
				return true;
			}
		}
	}
	return false;
}

pub fn run(text: []const u8) void {
	const heap = std.heap.page_allocator;
	const main_buffer = heap.alloc(u8, 0x100000000) catch unreachable;
	var main_mem_fixed = std.heap.FixedBufferAllocator.init(main_buffer);
	var main_mem = main_mem_fixed.allocator();
	const program_text = "";
	const tokens = tokenize(&main_mem, program_text);
	const input_tokens = tokenize(&main_mem, text);
	var i: u64 = 0;
	var program = parse(&main_mem, tokens.items, &i) catch unreachable;
	i = 0;
	var stream = parse(&main_mem, input_tokens.items, &i) catch unreachable;
	while (append_step(&program, &stream)){
		program.show();
		std.debug.print("\n", .{});
	}
}

pub fn idle() void {
	const heap = std.heap.page_allocator;
	const main_buffer = heap.alloc(u8, 0x100000000) catch unreachable;
	var main_mem_fixed = std.heap.FixedBufferAllocator.init(main_buffer);
	var main_mem = main_mem_fixed.allocator();
	const program_text = "";
	const tokens = tokenize(&main_mem, program_text);
	var i: u64 = 0;
	var program = parse(&main_mem, tokens.items, &i) catch unreachable;
	const stdin = std.io.getStdIn().reader();
	while (true){
		std.debug.print("> ", .{});
		program.show();
		var buf = main_mem.alloc(u8, 64) catch unreachable;
		const len = stdin.read(buf) catch unreachable;
		const input = buf[0..len];
		const stream_tokens = tokenize(&main_mem, input);
		i = 0;
		var stream = parse(&main_mem, stream_tokens.items, &i) catch unreachable;
		while (append_step(&program, &stream)){}
		program.show();
		std.debug.print("\n", .{});
	}
}

pub fn main() !void {
	idle();
}
