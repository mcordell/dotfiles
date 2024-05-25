local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require("luasnip.util.events")
local ai = require("luasnip.nodes.absolute_indexer")
local extras = require("luasnip.extras")
local l = extras.lambda
local rep = extras.rep
local p = extras.partial
local m = extras.match
local n = extras.nonempty
local dl = extras.dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local conds = require("luasnip.extras.expand_conditions")
local postfix = require("luasnip.extras.postfix").postfix
local types = require("luasnip.util.types")
local parse = require("luasnip.util.parser").parse_snippet
local ms = ls.multi_snippet
local k = require("luasnip.nodes.key_indexer").new_key



local get_visual = function(_args, parent)
  if (#parent.snippet.env.LS_SELECT_RAW > 0) then
    return sn(nil, i(1, parent.snippet.env.LS_SELECT_RAW))
  else -- If LS_SELECT_RAW is empty, return a blank insert node
    return sn(nil, i(1))
  end
end -- Place this in ${HOME}/.config/nvim/LuaSnip/all.lua

local snakeToCamelCase = function(snake_string)
  -- remove .rb extension
  -- convert snake case to camel case
  local camel_case_string = snake_string:gsub("(%l)(%w*)", function(a, b) return string.upper(a) .. b end)
  camel_case_string = camel_case_string:gsub("_", "")

  return camel_case_string
end

local spiltParameters = function(args, _parent, _user_args)
  local output = {}

  for param in args[1][1]:gmatch("([^, ]+)") do
    table.insert(output, "  @" .. param .. " = " .. param)
  end

  return output
end

local splitParamDoc = function(args, _parent, _user_args)
  local output = {}

  for param in args[1][1]:gmatch("([^, ]+)") do
    table.insert(output, "# @param " .. param .. " [Type] description")
  end

  return output
end

local function split(str, delimiter)
  local result = {}
  for match in (str .. delimiter):gmatch("(.-)" .. delimiter) do
    table.insert(result, match)
  end
  return result
end

local dynamicModuleName = function(_args, parent)
  local rel = parent.snippet.env.RELATIVE_FILEPATH
  local shorter = parent.snippet.env.TM_FILENAME
  -- Split the relative path
  local parts = split(rel, "/")

  -- Remove the file name from the list
  table.remove(parts)
  if parts[1] == "lib" then
    table.remove(parts, 1)
  end
  for it, v in ipairs(parts) do
    parts[it] = snakeToCamelCase(v)
  end
  local result = table.concat(parts, "::")
  return sn(nil, i(1, result))
end


local itblock =
    fmta(
      [[
      it '<>' do
        <>
      end
    ]],
      {
        i(1, "does something"),
        i(0)
      }
    )

local classNode = fmta(
  [[
      # <>
      class <>
          <>
      end
    ]],
  {
    i(2, "docstring"),
    d(1, function(_args, parent)
      local shorter = parent.snippet.env.TM_FILENAME:sub(1, -4)

      return sn(nil, i(1, snakeToCamelCase(shorter)))
    end),
    i(0)
  }
)

local get_class_node = function(_args, _parent)
  return sn(nil, classNode)
end

return {
  s(
    { trig = "rd", desc = "disable a rubocop cop for selected block" },
    fmta("#rubocop:disable <>\n<>\n#rubocop:enable <>",
      {
        i(1, "cop name 2"),
        d(2, get_visual),
        rep(1)
      }
    )
  ),
  s(
    { trig = "pry", desc = "disable a rubocop cop for selected block", snippetType = "autosnippet" },
    t("require 'pry'; binding.pry")
  ),
  s(
    { trig = "ise", desc = "it is_expected", snippetType = "autosnippet" },
    fmta("it { is_expected.to <> }",
      {
        i(0)
      }
    )
  ),
  s(
    { trig = "irb", desc = "bidning for irb" },
    t("binding.irb")
  ),
  s(
    { trig = ".map", desc = "map function", snippetType = "autosnippet", wordTrig = false },
    fmta(".map { |<>| <> }",
      {
        i(1, "obj"),
        i(0)
      }
    )
  ),
  s(
    { trig = ".reduce ", desc = "reduce function", snippetType = "autosnippet", wordTrig = false },
    fmta(".reduce(<>) { |<>,<>| <> }",
      {
        i(1, "object"),
        i(3, "memo"),
        i(2, "item"),
        i(0)
      }
    )
  ),
  s(
    { trig = "init", desc = "initializer", snippetType = "autosnippet", condition = conds.line_begin },
    fmt("{}\ndef initialize({})\n{}\nend",
      {
        f(splitParamDoc, { 1 }),
        i(1, "parameters"),
        f(spiltParameters, { 1 })
      }
    )
  ),
  s(
    { trig = "bool", desc = "define bool method", condition = conds.line_begin },
    fmt("# @return [Boolean] {}\ndef {}?\n  false\nend",
      {
        i(0, "description"),
        i(1, "method_name"),
      }
    )
  ),
  s(
    { trig = "def", desc = "define method", condition = conds.line_begin },
    fmt("def {}({})\n  {}\nend",
      {
        i(1, "method_name"),
        i(2, "parameters"),
        i(0)
      }
    )
  ),
  s(
    { trig = "defd", desc = "define delegators", condition = conds.line_begin },
    fmt("def_delegators :{}, {}",
      {
        i(1, "target"),
        i(0)
      }
    )
  ),
  s(
    { trig = "cont", desc = "cont", condition = conds.line_begin },
    fmt("context '{}' do\n  {}\nend",
      {
        i(1, "description"),
        i(0)
      }
    )
  ),
  s(
    { trig = "^%s*mod", desc = "module", wordTrig = false, regTrig = true, snippetType = "autosnippet" },
    fmta(
      [[
      # frozen_string_literal: true

      module <>
        <>
      end
    ]],
      {
        d(1, dynamicModuleName),
        d(2, get_class_node)
      },
      {
        merge_child_ext_opts = false
      }
    )
  ),
  s(
    { trig = "^%s*class", desc = "Class", wordTrig = false, regTrig = true, snippetType = "autosnippet" },
    vim.list_extend(
      {
        t({ "# frozen_string_literal: true", "", "" })
      },
      classNode
    )
  ),

  s(
    { trig = "frozen", desc = "frozen", snippetType = "autosnippet", condition = conds.line_begin },
    t("# frozen_string_literal: true")
  ),
  s(
    { trig = "rdesc", desc = "RSpec describe class", snippetType = "autosnippet" },
    fmta(
      [[
    # frozen_string_literal: true

    RSpec.describe <> do
      subject(:instance) { described_class.new }

      <>
    end
    ]],
      {
        d(1, function(_args, parent)
          local shorter = parent.snippet.env.TM_FILENAME:sub(1, -4)

          return sn(nil, i(1, snakeToCamelCase(shorter)))
        end),
        i(0)
      }
    )
  ),
  s(
    { trig = "desc", desc = "describe block" },
    fmta(
      [[
    describe '<>' do
      <>
    end
    ]],
      {
        i(1, "test description"),
        i(0),
      }
    )
  ),
  s(
    { trig = "let", desc = "let variable", snippetType = "autosnippet", condition = conds.line_begin },
    fmta("let(:<>) { <> }",
      {
        i(1, "var_name"),
        i(0)
      }
    )
  ),
  s(
    { trig = "subject", desc = "subject for rspesc", snippetType = "autosnippet", condition = conds.line_begin },
    fmta("subject(:<>) { <> }",
      {
        i(1, "subject_name"),
        i(0)
      }
    )
  ),
  s(
    { trig = "decm", desc = "describe block for class method", snippetType = "autosnippet", condition = conds.line_begin },
    fmta(
      [[
    describe '.<>' do
      subject(:<>) { described_class.<> }

      it 'does something' do
        <>
      end
    end
    ]],
      {
        i(1, "method_name"),
        i(2, "subject_name"),
        rep(1),
        i(0)
      }
    )
  ),

  s(
    {
      trig = "desi",
      desc = "describe block for instance method",
      snippetType = "autosnippet",
      condition = conds.line_begin
    },
    fmta(
      [[
    describe '#<>' do
      subject(:<>) { instance.<> }

      it "<>" do
        <>
      end
    end
    ]],

      {
        i(1, "method_name"),
        i(2, "subject_name"),
        rep(1),
        i(3, "does something"),
        i(0)
      }
    )
  ),
  s(
    {
      trig = "it",
      desc = "it block",
      snippetType = "autosnippet",
      condition = conds.line_begin
    },
    itblock
  ),
  s(
    { trig = "sh", desc = "yard api tag", snippetType = "autosnippet", condition = conds.line_begin },
    t("require 'spec_helper'")
  ),
  s(
    { trig = "atread", desc = "attr_reader with yard doc", snippetType = "autosnippet" },
    fmta(
      [[
    # @!attribute [r] <>
    #   @return [<>] <>
    attr_reader :<>
    ]],
      {
        rep(1),
        i(2, "Type"),
        i(0),
        i(1, "attribute_name")
      }
    )
  ),
  s(
    { trig = "atacc", desc = "attr_reader with yard doc", snippetType = "autosnippet" },
    fmta(
      [[
    # @!attribute [rw] <>
    #   @return [<>] <>
    attr_accessor :<>
    ]],
      {
        rep(1),
        i(2, "Type"),
        i(0),
        i(1, "attribute_name")
      }
    )
  ),
  s(
    { trig = "# api", desc = "yard api tag", snippetType = "autosnippet" },
    t("# @api private")
  ),
  s(
    { trig = "# param", desc = "yard parameter tag", snippetType = "autosnippet", },
    fmta("# @param <> [<>] <>",
      {
        i(1, "var_name"),
        i(2, "Type"),
        i(3, "description of var")
      }
    )
  ),
  s(
    { trig = "# ret", desc = "yard return tag", snippetType = "autosnippet" },
    fmta("# @return [<>] <>",
      {
        i(1, "Types"),
        i(0, "description"),
      }
    )
  ),
  s(
    { trig = "# rais", desc = "yard raise tag", snippetType = "autosnippet" },
    fmta("# @raise [<>] <>",
      {
        i(1, "Types"),
        i(0, "description"),
      }
    )
  ),
  s(
    { trig = "# opt", desc = "define method", snippetType = "autosnippet" },
    fmta(
      [[
      # @option opts [<>] :<> <>
    ]],
      {
        i(1, "OptType"),
        i(2, "opt_key"),
        i(3, "opt description")
      }
    )
  ),
  s(
    { trig = "# opth", desc = "define method", snippetType = "autosnippet" },
    fmta(
      [[
      # @param [Hash] opts <>
      # @option opts [<>] :<> <>
    ]],
      {
        i(1, "opts description"),
        i(2, "OptType"),
        i(3, "opt_key"),
        i(4, "opt description")
      }
    )
  )
}
