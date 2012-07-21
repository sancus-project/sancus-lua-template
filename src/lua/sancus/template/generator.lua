-- This file is part of sancus-lua-template
-- <https://github.com/sancus-project/sancus-lua-template>
--
-- Copyright (c) 2012, Alejandro Mery <amery@geeks.cl>
--

local lpeg = assert(require"lpeg")
local P,R,S,V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C,Cc,Cg,Ct,Cp = lpeg.C, lpeg.Cc, lpeg.Cg, lpeg.Ct, lpeg.Cp

local assert, setmetatable, type = assert, setmetatable, type

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

local function new(s)
	assert(type(s) == "string")

	local t, p = parser:match(s)
	if t and p == #s+1 then
		return t
	end
end

setmetatable(_M, {
	__call = function(_, s) return new(s) end,
})

return _M
