--[[ INFO ----------------------------------------

http://github.com/jasonrudolph/keyboard
http://github.com/dbmrq/dotfiles/

See 'hs.keycodes.map' for keycodes.

Check out:
- ControlEscape.spoon
- http://www.hammerspoon.org/Spoons/Seal.html
- https://medium.com/@robhowlett/hammerspoon-the-best-mac-software-youve-never-heard-of-40c2df6db0f8

]]

hs.window.animationDuration = 0
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
-- TODO:
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
hs.hotkey.bind(hyper, 'left', function() hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1}) end)
hs.hotkey.bind(hyper, 'right', function() hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1}) end)
hs.hotkey.bind(hyper, 'h', function() hs.window.focusedWindow():moveToUnit({0, 0, 0.333, 1}) end)
hs.hotkey.bind(hyper, 'j', function() hs.window.focusedWindow():moveToUnit({0, 0, 0.666, 1}) end)
hs.hotkey.bind(hyper, 'k', function() hs.window.focusedWindow():moveToUnit({0.333, 0, 0.666, 1}) end)
hs.hotkey.bind(hyper, 'l', function() hs.window.focusedWindow():moveToUnit({0.666, 0, 0.333, 1}) end)

-- window hints
hs.hotkey.bind(hyper, 'h', hs.hints.windowHints)

-- grid gui
hs.grid.setMargins({w = 0, h = 0})
hs.hotkey.bind(hyper, 'g', hs.grid.show)


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


-- Word Move/Delete
-- -----------------------------------------------

-- This, remember, is at the system level, so Terminal/Emacs will need to accept
-- things like alt+delete and cmd+delete.

hs.hotkey.bind({'ctrl'}, 'w', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
hs.hotkey.bind({'ctrl'}, 'u', function() hs.eventtap.keyStroke({'cmd'}, 'delete') end)
hs.hotkey.bind({'ctrl'}, ';', function() hs.eventtap.keyStroke({'ctrl', 'alt'}, 'b') end)
hs.hotkey.bind({'ctrl'}, "'", function() hs.eventtap.keyStroke({'ctrl', 'alt'}, 'f') end)
hs.hotkey.bind({'alt'}, 'd', function() hs.eventtap.keyStroke({'alt'}, 'forwarddelete') end)
