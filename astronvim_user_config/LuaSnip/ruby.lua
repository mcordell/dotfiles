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
    fmt("def initialize({})\n{}\nend",
      {
        i(1, "parameters"),
        f(spiltParameters, { 1 })
      }
    )
  ),
  s(
    { trig = "def", desc = "define method", snippetType = "autosnippet", condition = conds.line_begin },
    fmt("def {}({})\n  {}\nend",
      {
        i(1, "method_name"),
        i(2, "parameters"),
        i(0)
      }
    )
  ),
  s(
    { trig = "^%s*class", desc = "Class", wordTrig = false, regTrig = true, snippetType = "autosnippet" },
    fmta(
      [[
      class <>
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
    { trig = "desc", desc = "describe block", snippetType = "autosnippet" },
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
    { trig = "# retm", desc = "yard return tag", snippetType = "autosnippet" },
    fmta("# @return [<>] <>",
      {
        i(1, "Types"),
        i(0, "description"),
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
