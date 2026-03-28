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
	comp: ?Program
};

const Term = struct {
	fptr: []Instruction,
	trigger: []Instruction,
	result: []Instruction
};

const Program = struct {
	terms: Buffer(Term),
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
		.terms = Buffer(Term).init(mem.*),
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

pub fn main() !void {
	const heap = std.heap.page_allocator;
	const main_buffer = heap.alloc(u8, 0x100000000) catch unreachable;
	var main_mem_fixed = std.heap.FixedBufferAllocator.init(main_buffer);
	var main_mem = main_mem_fixed.allocator();
	const program_text = "[.x .y swp: y x] 1/(a b c) (2 3) swp";
	const tokens = tokenize(&main_mem, program_text);
	var i: u64 = 0;
	const program = parse(&main_mem, tokens.items, &i) catch unreachable;
	program.show();
	std.debug.print("\n", .{});
}
