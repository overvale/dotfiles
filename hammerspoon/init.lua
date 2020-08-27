--[[ INFO ----------------------------------------

KeyCodes:
f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
f16, f17, f18, f19, f20, pad, pad*, pad+, pad/, pad-, pad=,
pad0, pad1, pad2, pad3, pad4, pad5, pad6, pad7, pad8, pad9,
padclear, padenter, return, tab, space, delete, escape, help,
home, pageup, forwarddelete, end, pagedown, left, right, down, up,
shift, rightshift, cmd, rightcmd, alt, rightalt, ctrl, rightctrl,
capslock, fn

Inspiration:
- http://www.hammerspoon.org/Spoons/Seal.html
- https://medium.com/@robhowlett/hammerspoon-the-best-mac-software-youve-never-heard-of-40c2df6db0f8
- https://github.com/jasonrudolph/keyboard
- https://github.com/dbmrq/dotfiles
- https://github.com/raulchen/dotfiles
- https://github.com/justintanner/universal-emacs-keybindings

]] ---------------------------------------------------

hs.window.animationDuration = 0
-- TODO: file bug for this setting not always taking effect
local hyper = {"ctrl", "alt", "cmd"}

-- Spoons
-- -----------------------------------------------

-- Load SpoonInstall, so we can easily load our other Spoons
hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall.use_syncinstall = true
Install=spoon.SpoonInstall

local anycomplete = require "anycomplete"
anycomplete.registerDefaultBindings()

-- Draw pretty rounded corners on all screens
Install:andUse("RoundedCorners", { start = true })

-- Vim Mode
local VimMode = hs.loadSpoon('VimMode')
local vim = VimMode:new()
vim:bindHotKeys({ enter = {{'ctrl'}, '['} })
vim:disableForApp('Terminal')
vim:disableForApp('Emacs')
vim:shouldDimScreenInNormalMode(nil)

require("smart_modifier_keys")
-- You could create a modal hotkey that enters an "emacs god mode"
-- wherein all pressed keys have a ctrl modifier added to them.

-- Window Control
-- -----------------------------------------------

function snap_window(dir)
   local thiswindow = hs.window.frontmostWindow()
   local loc = thiswindow:frame()
   
   local thisscreen = thiswindow:screen()
   local screenrect = thisscreen:frame()
   
   if dir == 'left' then
      loc.x = 0
   elseif dir == 'right' then
      loc.x = screenrect.w - loc.w
   elseif dir == 'up' then
      loc.y = 0
   elseif dir == 'down' then
      loc.y = screenrect.h - loc.h
   end
   thiswindow:setFrame(loc)
end

-- Move windows around the screen
hs.hotkey.bind(hyper, "[", function() snap_window('left') end)
hs.hotkey.bind(hyper, "]", function() snap_window('right') end)
hs.hotkey.bind(hyper, '=', function() hs.window.centerOnScreen(hs.window.focusedWindow()) end)

-- Resize and Move Windows
-- It seems you have to press the keys a few times to get them to work
hs.hotkey.bind(hyper, 'left', function() hs.window.focusedWindow():moveToUnit({0, 0, 1/2, 1}) end)
hs.hotkey.bind(hyper, 'right', function() hs.window.focusedWindow():moveToUnit({1/2, 0, 1/2, 1}) end)
hs.hotkey.bind(hyper, 'h', function() hs.window.focusedWindow():moveToUnit({0, 0, 1/3, 1}) end)
hs.hotkey.bind(hyper, 'j', function() hs.window.focusedWindow():moveToUnit({0, 0, 2/3, 1}) end)
hs.hotkey.bind(hyper, 'k', function() hs.window.focusedWindow():moveToUnit({1/3, 0, 2/3, 1}) end)
hs.hotkey.bind(hyper, 'l', function() hs.window.focusedWindow():moveToUnit({2/3, 0, 1/3, 1}) end)
-- TODO: file bug for not all of these being applied on first hotkey press

-- window hints
hs.hotkey.bind(hyper, 'i', hs.hints.windowHints)

-- window grid
hs.grid.setGrid('6x4', nil, nil)
hs.grid.setMargins({0, 0})
hs.hotkey.bind(hyper, ';', hs.grid.show)


-- Reload Config
-- -----------------------------------------------

-- Notify when this file is loaded
hs.notify.new({title='Hammerspoon', informativeText='Ready to rock 🤘'}):send()

-- Create a binding for reloading the config
hs.hotkey.bind({'cmd', 'ctrl'}, 'r', function() hs.reload() end)

-- Automatically reload the config when anything in the hammerspoon dir changes
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


-- Application Launcher
-- -----------------------------------------------

local appList = {
   -- don't use f (full-screen app)
   s = 'Safari',
   t = 'Terminal',
   a = 'Music',
   m = 'Mail',
   e = 'Emacs',
   b = 'BBEdit',
   o = 'Tot',
   g = 'Gmail',
   k = 'Slack',
}

for key, app in pairs(appList) do
   hs.hotkey.bind({'ctrl', 'cmd'}, key, function() hs.application.launchOrFocus(app) end)
end


-- Word Move/Delete
-- -----------------------------------------------

-- This, remember, is at the system level, so Terminal/Emacs will need to accept
-- alt+delete, cmd+delete, ctrl+alt+b, ctrl+alt+f, and alt+forwarddelete

-- If I bind the keys in this way:
-- hs.hotkey.bind({'ctrl'}, 'w', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
-- hs.hotkey.bind({'ctrl'}, 'u', function() hs.eventtap.keyStroke({'cmd'}, 'delete') end)
-- hs.hotkey.bind({'ctrl'}, ';', function() hs.eventtap.keyStroke({'ctrl', 'alt'}, 'b') end)
-- hs.hotkey.bind({'ctrl'}, "'", function() hs.eventtap.keyStroke({'ctrl', 'alt'}, 'f') end)
-- hs.hotkey.bind({'alt'}, 'd', function() hs.eventtap.keyStroke({'alt'}, 'forwarddelete') end)
-- That seems to introduce a significant delay, the keyStoke is not fired on keyDown

-- Whereas assigning a function to keyDown eliminates that delay
-- like this:

function deleteWordBack()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('delete', true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end
hs.hotkey.bind({'ctrl'}, 'w', deleteWordBack)

function deleteWordForward()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('forwarddelete', true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end
hs.hotkey.bind({'alt'}, 'd', deleteWordForward)

function deleteLineBack()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, true):post()
   hs.eventtap.event.newKeyEvent('delete', true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, false):post()
end
hs.hotkey.bind({'ctrl'}, 'u', deleteLineBack)

function moveWordBack()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('b', true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, false):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end
hs.hotkey.bind({'ctrl'}, ';', moveWordBack)

function moveWordForward()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
   hs.eventtap.event.newKeyEvent('f', true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, false):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
end
hs.hotkey.bind({'ctrl'}, "'", moveWordForward)


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
    showThumbnails = false,
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
