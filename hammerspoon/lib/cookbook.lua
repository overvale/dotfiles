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



