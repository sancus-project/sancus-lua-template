-- This file is part of sancus-lua-template
-- <https://github.com/sancus-project/sancus-lua-template>
--
-- Copyright (c) 2012, Alejandro Mery <amery@geeks.cl>
--

local utils = assert(require"sancus.utils")
local Class = assert(require"sancus.object").Class
local generator = assert(require"sancus.template.generator")

local assert, loadstring, type = assert, loadstring, type
local setfenv = setfenv
local coroutine, io = coroutine, io
local pp, stderr = utils.pprint, utils.stderr

local _M = { _NAME = ...}
setfenv(1, _M)

local function yield(s)
	if s ~= nil then
		coroutine.yield(s)
	end
end

Template = Class{
	-- coroutines
	yield = yield,
}

-- returns path to template by name
function Template:lookup(name)
	if type(name) == "string" then
		return "./" .. name
	end
end

function Template:iter(name, context)
	local f,g
	if self.templates == nil then
		self.templates = {}
	end

	f = self:get(name)()
	setfenv(f, context)

	return function() f(self, context) end
end

function Template:load(key)
	local data
	local filename = assert(self:lookup(key))
	local f = assert(io.open(filename, "r"))

	data = assert(f:read("*all"))
	f:close()
	data = assert(generator(data))

	if self.templates == nil then
		self.templates = {}
	end

	self.templates[key] = data
	return data
end

function Template:get(key)
	assert(type(key) == "string", "key must be an string value")

	if self.templates == nil then
		self.templates = {}
	end

	return self.templates[key] or self:load(key)
end

function Template:include(context, args)
	if args and type(args.file) == "string" then
		local f = self:get(args.file)()
		setfenv(f, context)
		return f(self, context)
	end
end

return _M
