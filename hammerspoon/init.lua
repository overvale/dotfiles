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

-- These are variables which refer to different combinations of modifier keys.
local omega = {"ctrl", "cmd"}
local hyper = {"ctrl", "alt", "cmd"}
local super = {"ctrl", "alt", "cmd", "shift"}

-- Capture the hostname, so we can make this config behave differently
-- depending on the computer.
hostname = hs.host.localizedName()


-- Testing/Debugging
-- ----------------------------------------------

function genericSuccess()
   -- Function for creating a notification saying "Success!"
   -- This is useful when testing new Hammerspoon stuff.
   hs.notify.new({title='Hammerspoon', informativeText='Success!'}):send()
end

-- With the below you can run `open hammerspoon:///success` in your terminal
-- and you'll run the 'genericSuccess' function.
hs.urlevent.bind("success", genericSuccess)


-- Spoons
-- -----------------------------------------------

-- Anycomplete lets you insert text by using use Google search's autocomplete.
-- So incredibly handy for words you don't know how to spell.
local anycomplete = require "anycomplete"

-- The default binding for anycomplete is hyper-g
anycomplete.registerDefaultBindings()

-- This is a custom spoon I made that interacts with the command-line backup
-- tool 'restic' and some launchd scripts I run on my Mac. It's really great,
-- but not public at the moment. Maybe one day I'll package it up so people
-- can see it.
require('backup_menu')


-- Window Control
-- -----------------------------------------------

-- When moving/resizing windows, I don't want any animation, I just want them
-- to snap into position.
hs.window.animationDuration = 0

function snap_window(dir)
   -- Reposition the current window to the left, right, top, or bottom of screen.
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

-- Move windows left, right, center
hs.hotkey.bind(hyper, "[", function() snap_window('left') end)
hs.hotkey.bind(hyper, "]", function() snap_window('right') end)
hs.hotkey.bind(hyper, "=", function() hs.window.focusedWindow():centerOnScreen() end)

-- Move windows between monitors
hs.hotkey.bind(super, "[",  function() hs.window.focusedWindow():moveOneScreenWest(noResize, ensureInScreenBounds) end)
hs.hotkey.bind(super, "]",  function() hs.window.focusedWindow():moveOneScreenEast(noResize, ensureInScreenBounds)  end)

-- Half Screen
hs.hotkey.bind(hyper, 'left',   function() hs.window.focusedWindow():moveToUnit({0, 0, 1/2, 1}) end)
hs.hotkey.bind(hyper, 'right',  function() hs.window.focusedWindow():moveToUnit({1/2, 0, 1/2, 1}) end)

-- Thirds of Screen
hs.hotkey.bind(hyper, 'h', function() hs.window.focusedWindow():moveToUnit({0, 0, 1/3, 1}) end)
hs.hotkey.bind(hyper, 'j', function() hs.window.focusedWindow():moveToUnit({0, 0, 2/3, 1}) end)
hs.hotkey.bind(hyper, 'k', function() hs.window.focusedWindow():moveToUnit({1/3, 0, 2/3, 1}) end)
hs.hotkey.bind(hyper, 'l', function() hs.window.focusedWindow():moveToUnit({2/3, 0, 1/3, 1}) end)

-- Full Screen
hs.hotkey.bind(hyper, 'f', function() hs.window.focusedWindow():moveToUnit({0, 0, 1, 1}) end)
hs.hotkey.bind(hyper, 'up', function() hs.window.focusedWindow():moveToUnit({0, 0, 1, 1}) end)

-- window hints
hs.hotkey.bind(hyper, 'i', hs.hints.windowHints)

-- window grid
hs.grid.setGrid('8x4', nil, nil)
hs.grid.setMargins({0, 0})
hs.hotkey.bind(hyper, ';', hs.grid.show)


-- App Launcher
-- -----------------------------------------------

-- First, define a list of apps and the key you want to use to launch them.
local applicationHotkeys = {
   m = 'Mail',
   c = 'Calendar',
   e = 'Emacs',
   s = 'Safari',
   a = 'Music',
   t = 'Terminal',
}

-- Then loop over the list of apps and create a binding for each.
for key, app in pairs(applicationHotkeys) do
   hs.hotkey.bind(omega, key, function()
                     hs.application.launchOrFocus(app)
   end)
end


-- Readline Shortcuts
-- ----------------------------------------------
-- MacOS supports lots of Emacs-style shortcuts out of the box, but it is
-- missing M-f, M-b, M-d -- and I think it should also support the readline
-- shortcuts  C-u, C-w.
-- https://readline.kablamo.org/emacs.html
-- However, rather than just binding them globally, I want to switch them off
-- when Emacs and the Terminal are the foreground app, so the below code does
-- all that.

-- There are a lot of different ways to simulate key events but the below
-- approach, which simulates all the key up and down events for both modifiers
-- and the keys themselves has proved the most reliable and the least likely
-- to suffer from lag. At least that's what I've found.

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

local function appTitle()
   -- Get title of foreground app
   app = hs.application.frontmostApplication()
   if app ~= nil then
      return app:title()
   end
end

-- Create a new keymap
local ReadlineKeymap = hs.hotkey.modal.new()

local function setReadlineKeymap()
   -- Activate and deactivate keymap based on appTitle()
   if appTitle() == "Emacs" or appTitle() == "Terminal" then
      print('Readline keybindings OFF for ' .. appTitle())
      ReadlineKeymap:exit()
   else
      print('Readline keybindings ON for ' .. appTitle())
      ReadlineKeymap:enter()
   end
end

local function appWatcherFunction(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      setReadlineKeymap()
   end
end

setReadlineKeymap()
local appWatcher = hs.application.watcher.new(appWatcherFunction)
appWatcher:start()

ReadlineKeymap:bind({'ctrl'}, 'w', deleteWordBack)
ReadlineKeymap:bind({'ctrl'}, 'u', deleteLineBack)
ReadlineKeymap:bind({'alt'},  'd', deleteWordForward)
ReadlineKeymap:bind({'alt'},  'b', moveWordBack)
ReadlineKeymap:bind({'alt'},  'f', moveWordForward)


-- Toggle Dark Mode
-- ----------------------------------------------
-- Creates a function and binding for toggling MacOS's dark mode.

function darkModeStatus()
   -- return the status of Dark Mode
   local _, darkModeState = hs.osascript.javascript(
      'Application("System Events").appearancePreferences.darkMode()'
   )
   return darkModeState
end

function setDarkMode(state)
   -- Function for setting Dark Mode on/off.
   -- Argument should be either 'true' or 'false'.
   return hs.osascript.javascript(
      string.format(
         "Application('System Events').appearancePreferences.darkMode.set(%s)", state
   ))
end

function toggleDarkMode()
   -- Toggle Dark Mode status
   if darkModeStatus() then
      setDarkMode(false)
   else
      setDarkMode(true)
   end
end

hs.hotkey.bind(hyper, "d", toggleDarkMode)


-- My Hammerspoon Menubar Item
-- ----------------------------------------------

-- Name the menubar item
local myHammerMenu = hs.menubar.new()

-- Build the actual menubar item drop-down
function myHammerMenuItem()
   local snippetMenu = {
      { title = "waving hands around", fn = snipWave },
      { title = " ¯\\_(ツ)_/¯", fn = snipShrug },
      { title = "[Org Mode Date]", fn = snipOrgDate },
      { title = "[YYYY-MM-DD]", fn = snipISODate },
   }
   local menuTable = {
      { title = "Dark/Light Mode", fn = toggleDarkMode },
      { title = "Snippets", menu = snippetMenu },
      { title = "-" },
      { title = "Launch Emacs Debug Init", fn = emacsDebugInit },
      { title = "Launch Emacs Q", fn = emacsQ },
      { title = "-" },
      { title = "Copy Mail Message URL", fn = copyMailURL},
      { title = "New Mail Message", fn = newMailMessage },
      { title = "-" },
      { title = "Available Tools:", disabled = true },
      { title = "Any Complete", disabled = true },
      { title = "Snap Window", disabled = true  },
      { title = "Resize Window", disabled = true  },
      { title = "App Launcher", disabled = true  },

   }
   myHammerMenu:setMenu(menuTable)
end

function emacsDebugInit() os.execute( "~/Applications/Emacs.app/Contents/MacOS/Emacs --debug-init" ) end
function emacsQ() os.execute( "~/Applications/Emacs.app/Contents/MacOS/Emacs -q" ) end

function copyMailURL() os.execute( "~/home/dot/bin/getMailURL | pbcopy | open hammerspoon://success" ) end
function newMailMessage() os.execute("open mailto:") end

hs.hotkey.bind(hyper, "m", newMailMessage)

function snipWave() hs.eventtap.keyStrokes("(waving hands around)") end
function snipShrug() hs.eventtap.keyStrokes(" ¯\\_(ツ)_/¯") end
function snipOrgDate() hs.eventtap.keyStrokes(os.date("<%Y-%m-%d %a>")) end
function snipISODate() hs.eventtap.keyStrokes(os.date("%Y-%m-%d")) end

-- Add the menubar item to the menubar
myHammerMenuItem()

-- What do you want the menubar item to display?
myHammerMenu:setTitle("H")


-- Reload Notification
-- ----------------------------------------------

-- When this config is loaded, or reloaded, notify that it was done
-- successfully.

hs.notify.new({title='Hammerspoon', informativeText='🤘 Ready to Rock! 🤘'}):send()
hs.hotkey.bind(omega, 'r', hs.reload)


-- END HAMMERSPOON CONFIG --
