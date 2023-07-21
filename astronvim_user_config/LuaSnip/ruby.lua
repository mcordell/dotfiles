local get_visual = function(args, parent)
  if (#parent.snippet.env.LS_SELECT_RAW > 0) then
    return sn(nil, i(1, parent.snippet.env.LS_SELECT_RAW))
  else -- If LS_SELECT_RAW is empty, return a blank insert node
    return sn(nil, i(1))
  end
end -- Place this in ${HOME}/.config/nvim/LuaSnip/all.lua

function snakeToCamelCase(snake_string)
  -- remove .rb extension
  -- convert snake case to camel case
  local camel_case_string = snake_string:gsub("(%l)(%w*)", function(a, b) return string.upper(a) .. b end)
  camel_case_string = camel_case_string:gsub("_", "")

  return camel_case_string
end

return {
  -- A snippet that expands the trigger "hi" into the string "Hello, world!".
  s(
    { trig = "rd", desc = "disable a rubocop cop for selected block", snippetType = "autosnippet" },
    fmta("#rubocop:disable <>\n<>\n#rubocop:enable <>",
      {
        i(1, "cop name 2"),
        d(2, get_visual),
        rep(1)
      }
    )
  ),
  s(
    { trig = "def", desc = "define method", snippetType = "autosnippet" },
    fmta("def <>\n<>\nend",
      {
        i(1, "method_name"),
        i(0)
      }
    )
  ),
  s(
    { trig = "cla", desc = "define method", snippetType = "autosnippet" },
    fmta(
      [[
      class <>
          <>
      end
    ]],
      {
        d(1, function(args, parent)
          local s = parent.snippet.env.TM_FILENAME:sub(1, -4)

          return sn(nil, i(1, snakeToCamelCase(s)))
        end),
        i(0)
      }
    )
  ),
  s(
    { trig = "# api", desc = "yard api tag", snippetType = "autosnippet" },
    t("# @api private")
  ),
  s(
    { trig = "param", desc = "yard parameter tag", snippetType = "autosnippet" },
    fmta("# @param <> [<>] <>",
      {
        i(1, "var_name"),
        i(2, "Type"),
        i(3, "description of var")
      }
    )
  )
}
