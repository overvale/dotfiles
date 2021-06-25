-- Oliver Taylor's Hammerspoon Config
-- homepage: https://github.com/olivertaylor/dotfiles

-- Acceptable keycodes are here:
-- https://www.hammerspoon.org/docs/hs.keycodes.html#map

-- Inspiration:
-- + https://spinscale.de/posts/2016-11-08-creating-a-productive-osx-environment-hammerspoon.html
-- + https://medium.com/@robhowlett/hammerspoon-the-best-mac-software-youve-never-heard-of-40c2df6db0f8
-- + https://github.com/jasonrudolph/keyboard
-- + https://github.com/dbmrq/dotfiles
-- + https://github.com/raulchen/dotfiles
-- + https://github.com/justintanner/universal-emacs-keybindings


-- Config Setup
-- ----------------------------------------------

hs.window.animationDuration = 0
local omega = {"ctrl", "cmd"}
local hyper = {"ctrl", "alt", "cmd"}
local super = {"ctrl", "alt", "cmd", "shift"}


-- Capture the hostname, so we can make this config behave differently across my Macs
hostname = hs.host.localizedName()

function genericSuccess()
   hs.notify.new({title='Hammerspoon', informativeText='Success!'}):send()
end
hs.urlevent.bind("success", genericSuccess)

-- Spoons
-- -----------------------------------------------

local anycomplete = require "anycomplete"
anycomplete.registerDefaultBindings()

require('quick_menu')

if (hostname == "shadowfax") then
   require('backup_menu')
end


-- Window Control
-- -----------------------------------------------
-- All these shortcuts use the hyper key

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
hs.hotkey.bind(hyper, "=", function() hs.window.focusedWindow():centerOnScreen() end)

-- Move windows between monitors
hs.hotkey.bind(super, "[",  function() hs.window.focusedWindow():moveOneScreenWest(noResize, ensureInScreenBounds) end)
hs.hotkey.bind(super, "]",  function() hs.window.focusedWindow():moveOneScreenEast(noResize, ensureInScreenBounds)  end)

-- Resize and Move Windows
-- ------------------------------------------------
-- LEFT, TOP, RIGHT, BOTTOM
-- Halves
hs.hotkey.bind(hyper, 'left',   function() hs.window.focusedWindow():moveToUnit({0, 0, 1/2, 1}) end)
hs.hotkey.bind(hyper, 'right',  function() hs.window.focusedWindow():moveToUnit({1/2, 0, 1/2, 1}) end)
-- Thirds
hs.hotkey.bind(hyper, 'h', function() hs.window.focusedWindow():moveToUnit({0, 0, 1/3, 1}) end)
hs.hotkey.bind(hyper, 'j', function() hs.window.focusedWindow():moveToUnit({0, 0, 2/3, 1}) end)
hs.hotkey.bind(hyper, 'k', function() hs.window.focusedWindow():moveToUnit({1/3, 0, 2/3, 1}) end)
hs.hotkey.bind(hyper, 'l', function() hs.window.focusedWindow():moveToUnit({2/3, 0, 1/3, 1}) end)
hs.hotkey.bind(hyper, 'f', function() hs.window.focusedWindow():moveToUnit({0, 0, 1, 1}) end)


-- Reload Config
-- -----------------------------------------------

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


-- Launcher
-- -----------------------------------------------

local applicationHotkeys = {
   m = 'Mail',
   n = 'NetNewsWire',
   c = 'Calendar',
   e = 'Emacs',
   s = 'Safari',
   a = 'Music',
   o = 'Tot',
   b = 'BBEdit',
   t = 'Terminal',
}

for key, app in pairs(applicationHotkeys) do
   hs.hotkey.bind({'ctrl', 'cmd'}, key, function()
	 hs.application.launchOrFocus(app)
   end)
end


-- Reload Notification in Menubar
-- -----------------------------------------------
-- This places a temporary message in the menubar. Because it comes and goes,
-- it should probably be last in your config.

-- local HSNotifyMenu = hs.menubar.new()

-- function buildHSNotifyMenu()
-- -- When called, places the message in the menubar
-- -- and after a delay, calls a function to remove this item from the menubar
--    HSNotifyMenu:setTitle("🤘 Reloaded!")
--    hs.timer.doAfter(2, killHSNotifyMenu)
-- end

-- function killHSNotifyMenu()
--    HSNotifyMenu:delete()
-- end

-- -- Show when this file is evaluated
-- buildHSNotifyMenu()



-- END --

