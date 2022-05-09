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
-- + https://github.com/dbalatero/dotfiles/tree/master/hammerspoon


-- Misc Spoons
-- -----------------------------------------------

anycomplete = hs.loadSpoon("Anycomplete")
anycomplete.engine = "duckduckgo"
anycomplete.bindHotkeys()

-- This is a custom spoon I made that interacts with the command-line backup
-- tool 'restic' and some launchd scripts I run on my Mac. It's really great,
-- but not public at the moment. Maybe one day I'll package it up so people
-- can see it.
require('backup_menu')


-- Window Control Functions
-- -----------------------------------------------

hs.window.animationDuration = 0

function moveWindow(dir)
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

function moveResizeWin(loc)
   -- Move and resize window to my preferred locations
   if loc == 'left1/2' then
      hs.window.focusedWindow():moveToUnit({0, 0, 1/2, 1})
   elseif loc == 'right1/2' then
      hs.window.focusedWindow():moveToUnit({1/2, 0, 1/2, 1})
   elseif loc == 'left1/3' then
      hs.window.focusedWindow():moveToUnit({0, 0, 1/3, 1})
   elseif loc == 'left2/3' then
      hs.window.focusedWindow():moveToUnit({0, 0, 2/3, 1})
   elseif loc == 'right2/3' then
      hs.window.focusedWindow():moveToUnit({1/3, 0, 2/3, 1})
   elseif loc == 'right1/3' then
      hs.window.focusedWindow():moveToUnit({2/3, 0, 1/3, 1})
   elseif loc == 'full' then
      hs.window.focusedWindow():moveToUnit({0, 0, 1, 1})
   end
end

-- Named Window Movement/Resize Functions
function wm_left()        moveWindow('left') end
function wm_right()       moveWindow('right') end
function wm_center()      hs.window.focusedWindow():centerOnScreen() end
function wm_leftHalf()    moveResizeWin('left1/2') end
function wm_rightHalf()   moveResizeWin('right1/2') end
function wm_full()        moveResizeWin('full') end
function wm_left1third()  moveResizeWin('left1/3') end
function wm_left2third()  moveResizeWin('left2/3') end
function wm_right2third() moveResizeWin('right2/3') end
function wm_right1third() moveResizeWin('right1/3') end


-- Misc Functions
-- -----------------------------------------------

function genericSuccess()
   -- Function for creating a notification saying "Success!"
   -- This is useful when testing new Hammerspoon stuff.
   hs.notify.new({title='Hammerspoon', informativeText='Success!'}):send()
end

-- With the below you can run `open hammerspoon:///success` in your terminal
-- and you'll run the 'genericSuccess' function.
hs.urlevent.bind("success", genericSuccess)

local reloadHammerspoon = function() hs.reload() end

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


-- HyperKey.spoon
-- -----------------------------------------------

-- Load and create a new switcher with 'HyperKey.spoon'
local HyperKey = hs.loadSpoon("HyperKey")

-- Define Shortcut Keys
local hyper = {'cmd', 'alt', 'ctrl'}
local alpha = {'cmd', 'ctrl'}

-- Activate the shortcut keys
alphaKey = HyperKey:new(alpha)
hyperKey = HyperKey:new(hyper)

alphaKey
   :bind('m'):toApplication('/System/Applications/Mail.app')
   :bind('c'):toApplication('/System/Applications/Calendar.app')
   :bind('e'):toApplication('/Users/oht/Applications/Emacs.app')
   :bind('s'):toApplication('/Applications/Safari.app')
   :bind('a'):toApplication('/System/Applications/Music.app')
   :bind('t'):toApplication('/System/Applications/Utilities/Terminal.app')
   :bind('h'):toFunction("Reload Hammerspoon", reloadHammerspoon)
   :bind('l'):toFunction("Lock screen", hs.caffeinate.startScreensaver)

hyperKey
   :bind('d'):toFunction("Toggle Dark Mode", toggleDarkMode)
   :bind('['):toFunction("Move Win ←", wm_left)
   :bind(']'):toFunction("Move Win →", wm_right)
   :bind('='):toFunction("Center Window", wm_center)
   :bind('left'):toFunction("Move/Resize Win ◼︎◻︎", wm_leftHalf)
   :bind('right'):toFunction("Move/Resize Win ◻︎◼︎", wm_rightHalf)
   :bind('f'):toFunction("Move/Resize Win Fullscreen", wm_full)
   :bind('h'):toFunction("Move/Resize Win ◼︎◻︎◻︎", wm_left1third)
   :bind('j'):toFunction("Move/Resize Win ◼︎◼︎◻︎", wm_left2third)
   :bind('k'):toFunction("Move/Resize Win ◻︎◼︎◼︎", wm_right2third)
   :bind('l'):toFunction("Move/Resize Win ◻︎◻︎◼︎", wm_right1third)


-- Sky Rocket Spoon -- Window Resizing with Mouse
-- -----------------------------------------------

local SkyRocket = hs.loadSpoon("SkyRocket")

sky = SkyRocket:new({
  -- Opacity of resize canvas
  opacity = 0.25,

  -- Which mouse button to hold to move a window?
  moveMouseButton = 'left',
  -- Which mouse button to hold to resize a window?
  resizeMouseButton = 'left',

  -- Which modifiers to hold to move a window?
  moveModifiers = {'shift', 'ctrl'},
  -- Which modifiers to hold to resize a window?
  resizeModifiers = {'shift', 'alt'},
})


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


-- END HAMMERSPOON CONFIG --
