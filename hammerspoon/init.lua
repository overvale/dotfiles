--[[ Info ---------------------------------------------------------------------

KeyCodes:
f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
f16, f17, f18, f19, f20, pad, pad*, pad+, pad/, pad-, pad=,
pad0, pad1, pad2, pad3, pad4, pad5, pad6, pad7, pad8, pad9,
padclear, padenter, return, tab, space, delete, escape, help,
home, pageup, forwarddelete, end, pagedown, left, right, down, up,
shift, rightshift, cmd, rightcmd, alt, rightalt, ctrl, rightctrl,
capslock, fn

http://github.com/jasonrudolph/keyboard
http://github.com/dbmrq/dotfiles/

---------------------------------------------------------------------------- ]]

local anycomplete = require "anycomplete"
anycomplete.registerDefaultBindings()

-- This runs when this file is loaded to confirm it is working
hs.notify.new({title='Hammerspoon', informativeText='Ready to rock 🤘'}):send()

-- Automatically reload the config when the path changes
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
configWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()

-- this is just to make it easier to type 'keyStroke'
local function keyStrike(modifiers, key)
  return function() hs.eventtap.keyStroke(modifiers, key, 0) end
end


-- [ Application Launcher ] ---------------------------------------------------

local appList = {
	-- don't use f (full-screen app)
	s = 'Safari',
	t = 'Terminal',
	a = 'Music',
	m = 'Mail',
	e = 'Emacs',
	b = 'BBEdit',
}

for key, app in pairs(appList) do
	hs.hotkey.bind({'ctrl', 'cmd'}, key, function() hs.application.launchOrFocus(app) end)
end


-- [ Arrow Delete ] -------------------------------------------------------------

-- This makes it so fn+mods+left/right acts as a 'delete' modifer.
-- This way you can zip around with the arrow keys and add the 'fn' key to delete
-- while your hands are still on the arrows.

hs.hotkey.bind({},      'home', function() hs.eventtap.keyStroke({},      'delete') end)
hs.hotkey.bind({'alt'}, 'home', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
hs.hotkey.bind({'cmd'}, 'home', function() hs.eventtap.keyStroke({'cmd'}, 'delete') end)
hs.hotkey.bind({},      'end',  function() hs.eventtap.keyStroke({},      'forwarddelete') end)
hs.hotkey.bind({'alt'}, 'end',  function() hs.eventtap.keyStroke({'alt'}, 'forwarddelete') end)
hs.hotkey.bind({'cmd'}, 'end',  function() hs.eventtap.keyStroke({'ctrl'}, 'k') end)


-- [ Use space as hyper ] -----------------------------------------------------

-- Mostly I use this to remap space to control
-- but I can also use it to remap space+anything to whatever

-- setup the default state of the variables
hyper = false
hyperTime = nil

-- functions to check for modifiers
-- taken from here: https://gist.github.com/elliotwaite/f71a58d67479ecbfe641ca4ae642ee82
function hasCmd(event)
  local flags = event:getFlags()
  return flags.cmd and not (flags.alt or flags.ctrl or flags.shift)
end
function hasCmdShift(event)
  local flags = event:getFlags()
  return flags.cmd and flags.shift and not (flags.alt or flags.ctrl)
end
function hasNoFlags(event)
  local flags = event:getFlags()
  return not (flags.alt or flags.cmd or flags.ctrl or flags.shift)
end

--[[
This takes over all keyDown events in the system.
Yes, you're replacing your entire keyboard with this set of functions.
Good idea? Not sure. But performance seems fine.

Side effect 1: you lose key-repeat, but you can shift-space for that
Side effect 2: space is fired on keyUp, so there's a slight delay

This technique taken from here: https://github.com/dguo/dotfiles/blob/master/programs/hammerspoon/init.lua
]]
down = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(event)
  local character = event:getCharacters()
  local keyCode = event:getKeyCode()
  --print("down", character, keyCode)

  -- if the key in question is a space then set hyper to true
  -- must be written as a literal space
  if character == " " and hasNoFlags(event) then
    hyper = true
    if hyperTime == nil then
      hyperTime = hs.timer.absoluteTime()
    end
    return true
  end

  if character == 'j' and hyper then
    hs.eventtap.keyStroke({}, "left", 0)
    hyperTime = nil
    return true
  end
  
  if character == 'J' and hyper then
    hs.eventtap.keyStroke({}, "delete", 0)
    hyperTime = nil
    return true
  end
  
  if character == 'k' and hyper then
    hs.eventtap.keyStroke({}, "down", 0)
    hyperTime = nil
    return true
  end

  if character == 'i' and hyper then
    hs.eventtap.keyStroke({}, "up", 0)
    hyperTime = nil
    return true
  end

  if character == 'l' and hyper then
    hs.eventtap.keyStroke({}, "right", 0)
    hyperTime = nil
    return true
  end
  
  if character == 'L' and hyper then
    hs.eventtap.keyStroke({}, "forwarddelete", 0)
    hyperTime = nil
    return true
  end

  if character == 'h' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "a", 0)
    hyperTime = nil
    return true
  end

  if character == ';' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "e", 0)
    hyperTime = nil
    return true
  end

  if character == 'u' and hyper then
    hs.eventtap.keyStroke({'alt'}, "left", 0)
    hyperTime = nil
    return true
  end

  if character == 'o' and hyper then
    hs.eventtap.keyStroke({'alt'}, "right", 0)
    hyperTime = nil
    return true
  end

  if character == 'U' and hyper then
    hs.eventtap.keyStroke({'alt'}, "delete", 0)
    hyperTime = nil
    return true
  end

  if character == 'O' and hyper then
    hs.eventtap.keyStroke({'alt'}, "forwarddelete", 0)
    hyperTime = nil
    return true
  end

  if character == 'n' and hyper then
    hs.eventtap.keyStroke({'alt'}, "delete", 0)
    hyperTime = nil
    return true
  end

  if character == '/' and hyper then
    hs.eventtap.keyStroke({'alt'}, "forwarddelete", 0)
    hyperTime = nil
    return true
  end


end)
down:start()

up = hs.eventtap.new({hs.eventtap.event.types.keyUp}, function(event)
  local character = event:getCharacters()
  if character == " " and hyper then
      local currentTime = hs.timer.absoluteTime()
      -- print(currentTime, hyperTime)
      if hyperTime ~= nil and (currentTime - hyperTime) / 1000000 < 250 then
          down:stop()
          hs.eventtap.keyStrokes(" ")
          down:start()
      end
      hyper = false
      hyperTime = nil
  end
end)
up:start()
