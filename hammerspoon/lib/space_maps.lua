-- [ Use space as hyper ] -----------------------------------------------------

-- taken from here: https://gist.github.com/elliotwaite/f71a58d67479ecbfe641ca4ae642ee82

-- setup the default state of the variables
hyper = false
hyperTime = nil

-- functions to check for modifiers
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
  -- if you want to change this don't forget the 'up' function below
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



--[[ Alternate Maps ----------------

  if character == 'q' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "q", 0)
    hyperTime = nil
    return true
  end

  if character == 'w' and hyper then
    hs.eventtap.keyStroke({'alt'}, "delete", 0)
    hyperTime = nil
    return true
  end

  if character == 'e' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "e", 0)
    hyperTime = nil
    return true
  end

  if character == 'r' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "r", 0)
    hyperTime = nil
    return true
  end

  if character == 't' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "t", 0)
    hyperTime = nil
    return true
  end

  if character == 'y' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "y", 0)
    hyperTime = nil
    return true
  end

  if character == 'u' and hyper then
    hs.eventtap.keyStroke({'cmd'}, "delete", 0)
    hyperTime = nil
    return true
  end

  if character == 'i' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "i", 0)
    hyperTime = nil
    return true
  end

  if character == 'o' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "o", 0)
    hyperTime = nil
    return true
  end

  if character == 'p' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "p", 0)
    hyperTime = nil
    return true
  end
  
  if character == 'a' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "a", 0)
    hyperTime = nil
    return true
  end

  if character == 's' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "s", 0)
    hyperTime = nil
    return true
  end

  if character == 'd' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "d", 0)
    hyperTime = nil
    return true
  end

  if character == 'f' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "f", 0)
    hyperTime = nil
    return true
  end

  if character == 'g' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "g", 0)
    hyperTime = nil
    return true
  end

  if character == 'h' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "h", 0)
    hyperTime = nil
    return true
  end

  if character == 'j' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "j", 0)
    hyperTime = nil
    return true
  end

  if character == 'k' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "k", 0)
    hyperTime = nil
    return true
  end

  if character == 'l' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "l", 0)
    hyperTime = nil
    return true
  end

  if character == ';' and hyper then
    hs.eventtap.keyStroke({'alt'}, "left", 0)
    hyperTime = nil
    return true
  end

  if character == "'" and hyper then
    hs.eventtap.keyStroke({'alt'}, 'right', 0)
    hyperTime = nil
    return true
  end

  if character == 'z' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "z", 0)
    hyperTime = nil
    return true
  end

  if character == 'x' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "x", 0)
    hyperTime = nil
    return true
  end

  if character == 'c' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "c", 0)
    hyperTime = nil
    return true
  end

  if character == 'v' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "v", 0)
    hyperTime = nil
    return true
  end

  if character == 'b' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "b", 0)
    hyperTime = nil
    return true
  end

  if character == 'n' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "n", 0)
    hyperTime = nil
    return true
  end

  if character == 'm' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "m", 0)
    hyperTime = nil
    return true
  end

  if character == ',' and hyper then
    hs.eventtap.keyStroke({'alt'}, "delete", 0)
    hyperTime = nil
    return true
  end

  if character == '.' and hyper then
    hs.eventtap.keyStroke({'alt'}, "forwarddelete", 0)
    hyperTime = nil
    return true
  end

  if character == '/' and hyper then
    hs.eventtap.keyStroke({'ctrl'}, "/", 0)
    hyperTime = nil
    return true
  end

--]]
