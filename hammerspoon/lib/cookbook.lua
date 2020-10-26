--[[ NOTE:

This is a scratchpad with which I am learning hammerspoon.
Not all the code here actually works.

]]

-- [ Remap key only in Emacs ] ----------------------------------------------------

-- I don't know how to disable noise global key "Command + Shift +Q" in MacOS.
-- So i redirect "Command + Shift + Q" to "Ctrl + Command + Shift + Q" for Emacs,
-- then i make Emacs response "Ctrl + Command + Shift + Q" to implement key binding "Command + Shift + Q".
local newKeyEvent = require 'hs.eventtap'.event.newKeyEvent
local usleep = require 'hs.timer'.usleep
hs.hotkey.new(
    {"cmd", "shift"}, "q", nil,
    function()
        if window.focusedWindow():application():path() == "/Applications/Emacs.app" then
            local app = window.focusedWindow():application()

            newKeyEvent({"ctrl", "cmd", "shift"}, "q", true):post(app)
            usleep(1000)
            newKeyEvent({"ctrl", "cmd", "shift"}, "q", false):post(app)
        end
end):enable()


-- [ Keyboard Navigation, NOT EMACS ----------------------------------------------------

-- This code binds a series of keys when the frontmost application is NOT emacs.
-- It does this by watching the frontmost application and enabling/disabling
-- a series of bindings called noEmacsKeys
-- https://github.com/Hammerspoon/hammerspoon/issues/2081#issuecomment-668283868

-- This creates a modal hotkey object that can be turned on/on
-- by the application watcher.
-- Since you won't ever be triggering the model hotkey directly
-- set the hotkey to something that won't cause a conflict.
noEmacsKeys = hs.hotkey.modal.new({"cmd", "shift", "alt"}, "F19")
-- Now that the modal hotkey is created, you can attach the bindings
-- you'd like to activate in that 'mode'.
noEmacsKeys:bind({'ctrl'}, ',', keyStrike({'alt'}, 'left'))
noEmacsKeys:bind({'ctrl'}, '.', keyStrike({'alt'}, 'right'))
noEmacsKeys:bind({'ctrl'}, "'", keyStrike({'alt'}, 'forwarddelete'))
noEmacsKeys:bind({'ctrl'}, ';', keyStrike({'alt'}, 'delete'))
noEmacsKeeys:bind({'ctrl'}, 'u', keyStrike({'cmd'}, 'delete'))

-- Create an application watcher that deactivates the noEmacsKeys bindings
-- when an app whose name is Emacs activates.
function applicationWatcherCallback(appName, eventType, appObject)
  if (appName == "Emacs") then
    if (eventType == hs.application.watcher.activated) then
      -- Emacs just got focus, disable our hotkeys
      noEmacsKeys:exit()
    elseif (eventType == hs.application.watcher.deactivated) then
      -- Emacs just lost focus, enable our hotkeys
      noEmacsKeys:enter()
    end
  end
end

-- Create and start the application event watcher
watcher = hs.application.watcher.new(applicationWatcherCallback)
watcher:start()

-- Activate the modal state by default
noEmacsKeys:enter()

-- [ Learning! ]-------------------------------------------------------------

hs.hotkey.bind({'cmd', 'ctrl'}, '1', function() hs.alert.show("alert") end)

function oliverDown()
   hs.alert.show("down")
end
function oliverUp()
   hs.alert.show("up")
end

hs.hotkey.bind({'cmd', 'ctrl'}, '3', oliverDown, oliverUp)

function spaceDown()
   hs.alert.show("down")
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, true):post()
   hs.eventtap.event.newKeyEvent('e', true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, false):post()
end
function spaceUp()
   hs.eventtap.keyStroke({}, 'return', 0)
end

hs.hotkey.bind({'cmd', 'ctrl'}, '4', spaceDown, spaceUp)

-- [ Mode Test ] ---------------------------------------------------

function modeTestStart()
   hs.alert.show("Mode Entered")
   testModal:enter()
end
function modeTestEnd()
   hs.alert.show("Mode Exited")
   testModal:exit()
end

testModal = hs.hotkey.modal.new({'cmd', 'ctrl', 'alt'}, 'F18')
testModal:bind({''}, 'e', keyStrike({'ctrl'}, 'e'))
hs.hotkey.bind({'cmd', 'ctrl'}, '2', modeTestStart, modeTestEnd)

-- [ Application Watcher ] -------------------------------------------------------------

emacsGroup = hs.hotkey.modal.new({'cmd'}, 'f19')
emacsGroup:bind({'cmd', 'ctrl'}, '1', hs.alert.show('Emacs is active!'))

function activeAppWatcher(appName, eventType, appObject)
  if (appName == "Emacs") then
    if (eventType == hs.application.watcher.activated) then
      emacsGroup:exit()
    elseif (eventType == hs.application.watcher.deactivated) then
      emacsGroup:enter()
    end
  end
end

hs.application.watcher.new(activeAppWatcher):start()

-- Toggle Application
-- -------------------------------------------

mash = {'cmd', 'alt', 'ctrl'}

local function toggleApplication(name)
  local app = hs.application.find(name)
  if not app or app:isHidden() then
    hs.application.launchOrFocus(name)
  elseif hs.application.frontmostApplication() ~= app then
    app:activate()
  else
    app:hide()
  end
end

hs.hotkey.bind(mash, "c", function() toggleApplication("Google Chrome") end)
hs.hotkey.bind(mash, "d", function() toggleApplication("Dash") end)
hs.hotkey.bind(mash, "f", function() toggleApplication("Finder") end)
hs.hotkey.bind(mash, "g", function() toggleApplication("SourceTree") end)
hs.hotkey.bind(mash, "m", function() toggleApplication("Mail") end)
hs.hotkey.bind(mash, "p", function() toggleApplication("System Preferences") end)
hs.hotkey.bind(mash, "s", function() toggleApplication("Spotify") end)
hs.hotkey.bind(mash, "t", function() toggleApplication("Terminal") end)

-- VIM Mode
-- -----------------------------------------------

local normal = hs.hotkey.modal.new()

enterNormal = hs.hotkey.bind({"ctrl"}, "[", function()
      normal:enter()
      hs.alert.show('Normal mode')
end)

function left() hs.eventtap.keyStroke({}, "Left") end
normal:bind({}, 'h', left, nil, left)

function right() hs.eventtap.keyStroke({}, "Right") end
normal:bind({}, 'l', right, nil, right)

function up() hs.eventtap.keyStroke({}, "Up") end
normal:bind({}, 'k', up, nil, up)

function down() hs.eventtap.keyStroke({}, "Down") end
normal:bind({}, 'j', down, nil, down)

normal:bind({}, 'i', function()
    normal:exit()
    hs.alert.show('Insert mode')
end)

-- Arrow Delete
-- -----------------------------------------------

-- This makes it so fn+mods+left/right acts as a 'delete' modifer.
-- This way you can zip around with the arrow keys and add the 'fn' key to delete
-- while your hands are still on the arrows.

hs.hotkey.bind({},      'home', function() hs.eventtap.keyStroke({},      'delete') end)
hs.hotkey.bind({'alt'}, 'home', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
hs.hotkey.bind({'cmd'}, 'home', function() hs.eventtap.keyStroke({'cmd'}, 'delete') end)
hs.hotkey.bind({},      'end',  function() hs.eventtap.keyStroke({},      'forwarddelete') end)
hs.hotkey.bind({'alt'}, 'end',  function() hs.eventtap.keyStroke({'alt'}, 'forwarddelete') end)
hs.hotkey.bind({'cmd'}, 'end',  function() hs.eventtap.keyStroke({'ctrl'}, 'k') end)

-- Window Switcher
-- -----------------------------------------------
-- https://github.com/raulchen/dotfiles/tree/master/hammerspoon

local switcher = hs.window.switcher.new(nil, {
    fontName = ".AppleSystemUIFont",
    textSize = 16,
    textColor = { white = 0, alpha = 1 },
    highlightColor = { white = 0.5, alpha = 0.3 },
    backgroundColor = { white = 0.95, alpha = 0.9 },
    titleBackgroundColor = { white = 0.95, alpha = 0 },
    showThumbnails = true,
    showSelectedThumbnail = false,
})

local function nextWindow()
    switcher:next()
end

local function previousWindow()
    switcher:previous()
end

hs.hotkey.bind('alt', 'tab', nextWindow, nil, nextWindow)
hs.hotkey.bind('alt-shift', 'tab', previousWindow, nil, previousWindow)


-- Generic Function for newKeyEvent
-- -----------------------------------------------
-- https://github.com/Hammerspoon/hammerspoon/issues/1984#issuecomment-455317739

doKeyStroke = function(modifiers, character)
    if type(modifiers) == 'table' then
        local event = hs.eventtap.event

        for _, modifier in pairs(modifiers) do
            event.newKeyEvent(modifier, true):post()
        end

        event.newKeyEvent(character, true):post()
        event.newKeyEvent(character, false):post()

        for i = #modifiers, 1, -1 do
            event.newKeyEvent(modifiers[i], false):post()
        end
    end
end

hs.hotkey.bind({'alt'}, 'h', function() doKeyStroke({}, 'Left') end)


-- Bring all Finder windows to front when finder activated
-- -----------------------------------------------
function applicationWatcher(appName, eventType, appObject)
    if (eventType == hs.application.watcher.activated) then
        if (appName == "Finder") then
            -- Bring all Finder windows forward when one gets activated
            appObject:selectMenuItem({"Window", "Bring All to Front"})
        end
    end
end
appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()


-- Remap key per application
-- -----------------------------------------------
-- Remap a key one way in one application, and another in all others
-- LIMITATION: You can't remap a binding to the same binding,
-- hammerspoon will catch the keypress again and try to remap it again.
-- Hammerspoon also ALWAYS catches the bound key, so there's no way to
-- fall back to the same key that you've bound.
-- So leaving the 'else' statement below blank would result in
-- 'ctrl n' being a dead key outside Emacs.

hs.hotkey.bind({"ctrl"}, "j", function()
      local app = hs.application.frontmostApplication()
      if app:name() == "Emacs" then
	 hs.eventtap.keyStroke({"ctrl"}, "n")
      else
	 hs.eventtap.event.newKeyEvent({}, "down", true):post()
	 hs.eventtap.event.newKeyEvent({}, "down", false):post()
      end
end)

hs.hotkey.bind({"shift", "cmd"}, "/", function()
      local app = hs.application.frontmostApplication()
      if app:name() == "Emacs" then
	 hs.eventtap.event.newKeyEvent({"ctrl"}, "h", true):post()
	 hs.eventtap.event.newKeyEvent({"ctrl"}, "h", false):post()
      else
	 hs.application.frontmostApplication():selectMenuItem({"Help"})
      end
end)

-- Turning on/off a binding when app is focused
-- -----------------------------------------------

-- First, define what you want to happen when the hotkey is enabled
function sendReturn()
   hs.eventtap.event.newKeyEvent({}, "delete", true):post()
   hs.eventtap.event.newKeyEvent({}, "delete", false):post()
end

-- Then define the binding that you want to toggle
local returnKeyBinding = hs.hotkey.new({}, "return", sendReturn)

-- Now create, and start, an app watcher that enables/disables the binding
gameWatcher = hs.application.watcher.new(function(appName, eventType, appObject)
      if appName == "Emacs" then
	 if eventType == hs.application.watcher.activated then
	    returnKeyBinding:enable()
	 elseif eventType == hs.application.watcher.deactivated or eventType == hs.application.watcher.terminated then
	    returnKeyBinding:disable()
	 end
      end
end):start()
 
-- Another solution to the same problem...

yourBinding = hs.hotkey.bind(...)

local wf=hs.window.filter
xcodeWF = wf.new("Xcode")
xcodeWF:subscribe(wf.windowFocused, function()
  yourBinding:disable()
end):subscribe(wf.windowUnfocused, function()
  yourBinding:enable()
end)
