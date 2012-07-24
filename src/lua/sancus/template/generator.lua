-- This file is part of sancus-lua-template
-- <https://github.com/sancus-project/sancus-lua-template>
--
-- Copyright (c) 2012, Alejandro Mery <amery@geeks.cl>
--

local lpeg = assert(require"lpeg")
local P,R,S,V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C,Cc,Cg,Ct,Cp = lpeg.C, lpeg.Cc, lpeg.Cg, lpeg.Ct, lpeg.Cp

local assert, setmetatable, tostring, type = assert, setmetatable, tostring, type
local sformat = string.format

local _M = {}

local function parser()
	local space, nl = S" \t", P"\n" + P"\r\n"
	local rest = (1 - nl)^1
	local eol = (P(-1) + nl)

	local char = R"az" + R"AZ"
	local num = R"09"

	local dq = P'"'

	local be, ee = P"${", P"}"
	local bi, ei = P"<%", P"%>"
	local bc, ec = bi, P"/>"

	local attr = char * (char + num + P"_")^0
	local value = (dq * C((1-dq)^0) * dq) + (num^1)/tonumber

	-- ${ ... }
	local expression = be * C((1 - nl - ee)^1) * ee
	expression = Ct(Cg(expression, "value") * Cg(Cc("expr"), "type"))

	-- <%command ... />
	local command = space^1 * C(attr) * P"=" * value
	command = bc * Cg(attr, "value") * Cg(Cc("command"), "type") * (command^0) * space^0 * ec
	command = Ct(command)

	-- <% ... %>
	local inline = (1 - ei)^1
	inline = bi * (C(
		(space * inline) + (nl * inline)
		)+(space + nl)^1) * ei
	inline = Ct(Cg(inline, "value") * Cg(Cc("inline"), "type"))

	-- [ \t]* %% comment
	local comment = space^0 * P"%%" * rest * eol

	-- [ \t]* % ...
	local code = space^0 * P"%" * C(rest) * eol
	code = Ct(Cg(code, "value") * Cg(Cc("code"), "type"))

	-- everything else
	local content = 1 - nl - be - bi
	content = content^1 * eol^-1
	content = C(nl + content)

	local things = comment + code +
			content + expression + inline +
			command

	return Ct(things^1) * Cp()
end
parser = parser()

local function find_line_col_by_pos(s, p)
	local nl = P"\n" + P"\r\n"
	local content = (1-nl)^1
	local patt = (nl + (content * nl))*Cp()
	local t = Ct(patt^0):match(s)

	local line_no, from, to = 1, 0, nil

	for _, q in ipairs(t) do
		if q <= p then
			line_no = line_no + 1
			from = q
		else
			to = q
			break
		end
	end

	local col = p - from
	local s1, s2 = s:sub(from, to), ""
	s1 = C(content):match(s1)

	if col > 0 then
		s2 = s1:sub(1,col):gsub("[^\t]", " ") .. "^"
	else
		s2 = "^"
	end

	return line_no, col+1, s1, s2
end

local function parse(s)
	assert(type(s) == "string", "argument must be an string")

	if #s == 0 then
		return {}
	end

	local t, p = parser:match(s)
	if t and p == #s+1 then
		return t
	elseif p == nil then
		p = 1
	end

	-- useful error message
	local line, col, s1, s2 = find_line_col_by_pos(s, p)
	local fmt = "sancus.template.parse: invalid element at (%s, %d):\n%s:%s\n%s%s"
	local prefix

	line = tostring(line)
	prefix = (" "):rep(#line+1)

	error(fmt:format(line, col, line, s1, prefix, s2))
end

local function new(s)
	local t = parse(s)
	return t
end

setmetatable(_M, {
	__call = function(_, s) return new(s) end,
})

return _M
