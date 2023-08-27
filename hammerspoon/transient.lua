-- Transient Keymap
-- ---------------------------------------------
-- This creates a transient keymap, and cheatsheet, that exits
-- after a binding is triggered.

local transientBG
local transientBGColor = {red=0, green=0.4, blue=0.956}
local transientText
local transientTextColor = { red = 1, blue = 1, green = 1 }

local keySymbols = {
    cmd = "⌘",
    ctrl = "⌃"
    -- Add other replacements here
}

transientBindings = {}
transientKeys = hs.hotkey.modal.new()

-- Function for showing a cheatsheet of all bindings in the keymap
function showKeyBindings()
    -- Convert the table to a displayable string
    local displayString = "Transient Keymap\n\n"
    local maxWidth = 0
    for _, binding in ipairs(transientBindings) do
        local keys = binding[1]
        -- replace binding names with pretty symbols
        for i, key in ipairs(keys) do
            keys[i] = keySymbols[key] or key
        end
        keys = table.concat(keys, "") .. " " .. binding[2]
        -- Convert function names into displayable names
        local action = binding[3]
        if type(action) == "function" then
            local name
            for k, v in pairs(_G) do
                if v == action then
                    name = k
                    break
                end
            end
            action = name or "Unknown Function"
        end

        local line = keys .. "  " .. tostring(action) .. "\n"
        if #line * 9 > maxWidth then maxWidth = #line * 9 end
        displayString = displayString .. line
    end

    local lines = select(2, displayString:gsub('\n', '\n')) + 1
    local height = lines * 18 -- Adjust line height here

    -- Create and position a background rectangle
    transientBG = hs.drawing.rectangle(hs.geometry.rect(19, 33, maxWidth, height))
    transientBG:setFillColor(transientBGColor)

    -- Create and position a textbox
    local styledTextAttributes = {
        font = { name = "SF Mono", size = 14 },
        color = transientTextColor,
        paragraphStyle = { alignment = "left" }
    }
    local styledText = hs.styledtext.new(displayString, styledTextAttributes)
    transientText = hs.drawing.text(hs.geometry.rect(29, 43, maxWidth, height), styledText)
    
    -- Show the text
    transientBG:show()
    transientText:show()
end

-- Function for hiding the cheatsheet
function hideKeyBindings()
    transientBG:delete()
    transientText:delete()
end

function transientKeys:entered()
  myKeysMenuItem = hs.menubar.new():setTitle("􀇳 Transient Keymap!")
  myKeysMenuItem:setTooltip("Press Escape to deactivate.")
  showKeyBindings()
end

function transientKeys:exited()
  myKeysMenuItem:delete()
  hideKeyBindings()
end

-- Bind keys for activating the transient keymap and cheetsheat
do
   local mod = {'ctrl'}
   local key = 'return'
   hs.hotkey.bind(mod, key, function() transientKeys:enter() end)
   -- prevent recursion
   transientKeys:bind(mod, key, function() transientKeys:exit() end)
   -- escape should work as a way to exit
   transientKeys:bind('', 'escape', function() transientKeys:exit() end)
end

-- Set the kindings in keymap
function transientSetBindings()
	for i, mapping in ipairs(transientBindings) do
	  local mod = mapping[1]
	  local key = mapping[2]
	  local fn  = mapping[3]
	  transientKeys:bind(mod, key, function()
	  if (type(fn) == 'string') then
		hs.application.launchOrFocus(fn)
	  else
		fn()
	  end
	  transientKeys:exit() end)
	end
end