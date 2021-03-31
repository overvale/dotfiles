-- Functions
-- -----------------------------------------------
-- Put the key events into functions

function deleteLineBack()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, true):post()
   hs.eventtap.event.newKeyEvent('delete', true):post()
   hs.eventtap.event.newKeyEvent('delete', false):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, false):post()
end

function deleteWordBack()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('delete', true):post()
   hs.eventtap.event.newKeyEvent('delete', false):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end

function deleteWordForward()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('forwarddelete', true):post()
   hs.eventtap.event.newKeyEvent('forwarddelete', false):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end

function moveWordBack()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('left', true):post()
   hs.eventtap.event.newKeyEvent('left', false):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end

function moveWordForward()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('right', true):post()
   hs.eventtap.event.newKeyEvent('right', false):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end

function selectWordForward()
   hs.eventtap.event.newKeyEvent({"alt", "shift"}, "right", true):post()
   hs.eventtap.event.newKeyEvent({"alt", "shift"}, "right", false):post()
end

function forwardUpperCase()
   -- Make the next word UPPER CASE
   local currentApp = hs.application.frontmostApplication()
   selectWordForward()
   currentApp:selectMenuItem("Make Upper Case")
   -- hs.eventtap.event.newKeyEvent({}, "right", true):post()
   -- hs.eventtap.event.newKeyEvent({}, "right", false):post()
end

function forwardLowerCase()
   -- Make the next word lower case
   local currentApp = hs.application.frontmostApplication()
   selectWordForward()
   currentApp:selectMenuItem("Make Lower Case")
   -- hs.eventtap.event.newKeyEvent({}, "right", true):post()
   -- hs.eventtap.event.newKeyEvent({}, "right", false):post()
end

function forwardCapitalize()
   -- Make the next word Capitalized
   local currentApp = hs.application.frontmostApplication()
   selectWordForward()
   currentApp:selectMenuItem("Capitalize")
   -- hs.eventtap.event.newKeyEvent({}, "right", true):post()
   -- hs.eventtap.event.newKeyEvent({}, "right", false):post()
end

function selectLine()
   -- move to beginning of line
   hs.eventtap.event.newKeyEvent({"ctrl"}, "a", true):post()
   hs.eventtap.event.newKeyEvent({"ctrl"}, "a", false):post()
   -- Shift + move to end of line
   hs.eventtap.event.newKeyEvent({"shift", "cmd"}, "right", true):post()
   hs.eventtap.event.newKeyEvent({"shift", "cmd"}, "right", false):post()
end

function selectWord()
   -- move back word
   hs.eventtap.event.newKeyEvent({"alt"}, "left", true):post()
   hs.eventtap.event.newKeyEvent({"alt"}, "left", false):post()
   -- Shift + move forward word
   hs.eventtap.event.newKeyEvent({"shift", "alt"}, "right", true):post()
   hs.eventtap.event.newKeyEvent({"shift", "alt"}, "right", false):post()
end


-- General Bindings
-- -----------------------------------------------
-- Next, bind keys to the functions
-- These bindings are enabled by default

local bindDeleteWordBack    = hs.hotkey.bind({'ctrl'}, 'w', deleteWordBack)
local bindDeleteWordForward = hs.hotkey.bind({'alt'},  'd', deleteWordForward)
local bindDeleteLineBack    = hs.hotkey.bind({'ctrl'}, 'u', deleteLineBack)

local bindMoveWordBack      = hs.hotkey.bind({'alt'}, 'b', moveWordBack)
local bindMoveWordForward   = hs.hotkey.bind({'alt'}, "f", moveWordForward)
local bindforwardCapitalize = hs.hotkey.bind({'alt'}, 'c', forwardCapitalize)
local bindforwardLowerCase  = hs.hotkey.bind({'alt'}, "l", forwardLowerCase)
local bindforwardUpperCase  = hs.hotkey.bind({'alt'}, "u", forwardUpperCase)

local bindSelectLine        = hs.hotkey.bind({'ctrl', 'cmd'}, "l", selectLine)
local bindSelectWord        = hs.hotkey.bind({'ctrl', 'cmd'}, "w", selectWord)


-- App-Specific Bindings
-- -----------------------------------------------

-- Remote Desktop
local msrdDisable = hs.hotkey.new({"cmd"}, "w", function()
      -- nil
end)

-- Excel
local excelDown = hs.hotkey.new({"ctrl"}, "n", function()
   hs.eventtap.event.newKeyEvent({}, "down", true):post()
   hs.eventtap.event.newKeyEvent({}, "down", false):post()
end)
local excelUp = hs.hotkey.new({"ctrl"}, "p", function()
   hs.eventtap.event.newKeyEvent({}, "up", true):post()
   hs.eventtap.event.newKeyEvent({}, "up", false):post()
end)

msrdDisable:disable()
excelDown:disable()
excelUp:disable()


-- Application Watcher
-- -----------------------------------------------
-- Setup an application watcher that enables/disables the above bindings.

function applicationWatcher(appName, eventType, appObject)
   if appName == "Microsoft Remote Desktop" then
      if eventType == hs.application.watcher.activated then
         msrdDisable:enable()
      elseif eventType == hs.application.watcher.deactivated or eventType == hs.application.watcher.terminated then
         msrdDisable:disable()
      end
   elseif appName == "Microsoft Excel" then
      if eventType == hs.application.watcher.activated then
         excelUp:enable()
         excelDown:enable()
      elseif eventType == hs.application.watcher.deactivated or eventType == hs.application.watcher.terminated then
         excelUp:disable()
         excelDown:disable()
      end
   elseif appName == "Emacs" or appName == "Terminal" then
      if eventType == hs.application.watcher.activated then
	 bindDeleteWordBack:disable()
	 bindDeleteWordForward:disable()
	 bindDeleteLineBack:disable()
	 bindMoveWordBack:disable()
	 bindDeleteWordBack:disable()
	 bindMoveWordForward:disable()
	 bindDeleteWordForward:disable()
	 bindSelectLine:disable()
	 bindSelectWord:disable()
	 bindforwardCapitalize:disable()
	 bindforwardLowerCase:disable()
	 bindforwardUpperCase:disable()
      elseif eventType == hs.application.watcher.deactivated or eventType == hs.application.watcher.terminated then
	 bindDeleteWordBack:enable()
	 bindDeleteWordForward:enable()
	 bindDeleteLineBack:enable()
	 bindMoveWordBack:enable()
	 bindDeleteWordBack:enable()
	 bindMoveWordForward:enable()
	 bindDeleteWordForward:enable()
	 bindSelectLine:enable()
	 bindSelectWord:enable()
	 bindforwardCapitalize:enable()
	 bindforwardLowerCase:enable()
	 bindforwardUpperCase:enable()
      end
   end
end

appKeyBinder = hs.application.watcher.new(applicationWatcher)
appKeyBinder:start()
