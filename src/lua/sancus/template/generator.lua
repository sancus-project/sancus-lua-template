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

	local be, ee = P"${", P"}"
	local bi, ei = P"<%", P"%>"

	-- expression = ${ ... }
	local expression = be * C((1 - nl - ee)^1) * ee
	expression = Ct(Cg(expression, "value") * Cg(Cc("expr"), "type"))

	-- <% ... %>
	local inline = (1 - ei)^1
	inline = bi * (C(
		(space * inline) + (nl * inline)
		)+(space + nl)^1) * ei
	inline = Ct(Cg(inline, "value") * Cg(Cc("inline"), "type"))

	-- [ \t* %% comment
	local comment = space^0 * P"%%" * rest * eol
	-- [ \t]* % [ \t...
	local code = space^0 * P"%" * C(rest) * eol

	-- everything else
	local content = 1 - nl - be - bi
	content = content^1 * eol^-1
	content = C(nl + content)

	local things = comment + code +
			content + expression + inline

	return Ct(things^1) * Cp()
end
parser = parser()

local function find_nl()
	local nl = P"\n" + P"\r\n"
	local eos = P(-1)

	local line = (nl + ((1-nl)^1 * nl))*Cp()
	return Ct(line^0)
end
find_nl = find_nl()

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
	local line, from, to = 1, 0, nil
	t = find_nl:match(s)
	for _, q in ipairs(t) do
		if q <= p then
			line = line + 1
			from = q
		else
			to = q-2
			break
		end
	end
	local col = p - from
	local line = tostring(line)

	local s1 = s:sub(from, to)
	local s2 = (" "):rep(#line+1)

	if col > 0 then
		s2 = s2 ..  s1:gsub("[^\t]", " "):sub(1,col)
	end
	s2 = s2 .. "^"

	error(sformat("sancus.template.parse: invalid element at (%s,%d):\n%s:%s\n%s",
		line, col+1, line, s1, s2), 2)
end

local function new(s)
	local t = parse(s)
	return t
end

setmetatable(_M, {
	__call = function(_, s) return new(s) end,
})

return _M
