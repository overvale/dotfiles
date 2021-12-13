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

-- This is a custom spoon I make that interacts with the command-line backup
-- tool 'restic' and some launchd scripts I run on my Mac. It's really great,
-- but not public at the moment. Maybe one day I'll package it up so people
-- can see it.
require('backup_menu')


-- Window Control
-- -----------------------------------------------

-- Reposition the current window to the left, right, top, or bottom of screen.
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


-- Resize/Move Windows with Mouse
-- -----------------------------------------------

-- Inspired by Linux alt-drag or Better Touch Tools move/resize functionality
-- from https://gist.github.com/kizzx2/e542fa74b80b7563045a
-- Command-shift-move: move window under mouse
-- Alt-Shift-move: resize window under mouse
function get_window_under_mouse()
   local my_pos = hs.geometry.new(hs.mouse.getAbsolutePosition())
   local my_screen = hs.mouse.getCurrentScreen()
   return hs.fnutils.find(hs.window.orderedWindows(), function(w)
                             return my_screen == w:screen() and
                                w:isStandard() and
                                (not w:isFullScreen()) and
                                my_pos:inside(w:frame())
   end)
end

dragging = {}                   -- global variable to hold the dragging/resizing state

drag_event = hs.eventtap.new({ hs.eventtap.event.types.mouseMoved }, function(e)
      if not dragging then return nil end
      if dragging.mode==3 then -- just move
         local dx = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
         local dy = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)
         dragging.win:move({dx, dy}, nil, false, 0)
      else -- resize
         local pos=hs.mouse.getAbsolutePosition()
         local w1 = dragging.size.w + (pos.x-dragging.off.x)
         local h1 = dragging.size.h + (pos.y-dragging.off.y)
         dragging.win:setSize(w1, h1)
      end
end)

flags_event = hs.eventtap.new({ hs.eventtap.event.types.flagsChanged }, function(e)
      local flags = e:getFlags()
      local mode=(flags.shift and 1 or 0) + (flags.cmd and 2 or 0) + (flags.alt and 4 or 0)
      if mode==3 or mode==5 then -- valid modes
         if dragging then
            if dragging.mode == mode then return nil end -- already working
         else
            -- only update window if we hadn't started dragging/resizing already
            dragging={win = get_window_under_mouse()}
            if not dragging.win then -- no good window
               dragging=nil
               return nil
            end
         end
         dragging.mode = mode   -- 3=drag, 5=resize
         if mode==5 then
            dragging.off=hs.mouse.getAbsolutePosition()
            dragging.size=dragging.win:size()
         end
         drag_event:start()
      else                      -- not a valid mode
         if dragging then
            drag_event:stop()
            dragging = nil
         end
      end
      return nil
end)
flags_event:start()


-- Launcher
-- -----------------------------------------------

local applicationHotkeys = {
   m = 'Mail',
   c = 'Calendar',
   e = 'Emacs',
   s = 'Safari',
   a = 'Music',
   t = 'Terminal',
}

for key, app in pairs(applicationHotkeys) do
   hs.hotkey.bind(omega, key, function()
	 hs.application.launchOrFocus(app)
   end)
end


-- Toggle Dark Mode
-- ----------------------------------------------

function darkModeStatus()
  local _, darkModeState = hs.osascript.javascript(
    'Application("System Events").appearancePreferences.darkMode()'
  )
  return darkModeState
end

function setDarkMode(state)
  return hs.osascript.javascript(
    string.format(
      "Application('System Events').appearancePreferences.darkMode.set(%s)", state
    ))
end

function toggleDarkMode()
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

