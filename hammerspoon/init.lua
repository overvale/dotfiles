--[[ Oliver Taylor's Hammerspoon Config
homepage: https://github.com/olivertaylor/dotfiles

Acceptable keycodes are here:
https://www.hammerspoon.org/docs/hs.keycodes.html#map

Inspiration:
https://spinscale.de/posts/2016-11-08-creating-a-productive-osx-environment-hammerspoon.html
https://medium.com/@robhowlett/hammerspoon-the-best-mac-software-youve-never-heard-of-40c2df6db0f8
https://github.com/jasonrudolph/keyboard
https://github.com/dbmrq/dotfiles
https://github.com/raulchen/dotfiles
https://github.com/justintanner/universal-emacs-keybindings
https://github.com/dbalatero/dotfiles/tree/master/hammerspoon
https://github.com/senorprogrammer/hammerspoon_init/

--]]

-- Setup
-- -----------------------------------------------

local hyper = {'cmd', 'alt', 'ctrl'}
local alpha = {'cmd', 'ctrl'}
local power = {'cmd', 'shift'}

hs.window.animationDuration = 0

function keyUpDown(modifiers, key)
  hs.eventtap.keyStroke(modifiers, key, 0)
end

require("private")
require("macos-toggles")
require("functions")
require("hammer-menu")
require("backup-menu")

anycomplete = hs.loadSpoon("Anycomplete")
anycomplete.engine = "duckduckgo"
anycomplete.bindHotkeys()


-- Key Bindings
-- ----------------------------------------------

-- Accepts only function names
keyBindings = {
   -- hyper g reserved for Anycomplete
   { hyper, 's', toggleStageMan },
   { hyper, 'd', toggleDarkMode },
   { hyper, 'v', pastePlainText },
   { {'alt', 'cmd'}, 'm', toggleMenubar },
}

-- Accepts strings and function names
-- Strings are assumed to be Application names
transientKeysBindings = {
  { {'cmd'}, 'm', 'Mail' },
  { {'cmd'}, 'c', 'Calendar' },
  { {'cmd'}, 'b', 'BBEdit' },
  { {'cmd'}, 'n', 'Notes'},
  { {'cmd'}, 's', 'Safari' },
  { {'cmd'}, 'a', 'Music' },
  { {'cmd'}, 't', 'Terminal' },
  { {'cmd'}, 'r', 'Reminders' },
  { {'cmd'}, 'f', newFinderWindow },
  { {'cmd'}, 'g', searchGoogle },
  { {'cmd'}, 'd', todoTXT },
  { {'cmd'}, 'l', logbook },
}


hammerMenuTable = {
  -- { title = "", fn = },
  { title = "Toggle Dark Mode", fn = toggleDarkMode },
  { title = "Toggle Stage Manager", fn = toggleStageMan },
  { title = "Toggle Menu Bar", fn = toggleMenubar },
  { title = "-" },
  { title = "todo.txt", fn = todoTXT},
  { title = "Today's Log", fn = logbook },
  { title = "-" },
  { title = "Snippets", menu = {
    { title = "(waving hands around)", fn = snipWave },
    { title = " ¯\\_(ツ)_/¯", fn = snipShrug },
    { title = "<YYYY-MM-DD DDD>", fn = snipOrgDate },
    { title = "YYYY-MM-DD", fn = snipISODate },
    }
  },
  { title = "Search", menu = {
    { title = "Search YouTube", fn = searchYouTube },
    { title = "Search GitHub", fn = searchGitHub },
    { title = "Search Wikipedia", fn = searchWikipedia },
    { title = "Search IMDB", fn = searchIMDB },
    { title = "Search Wolfram Alpha", fn = searchWolframAlpha },
    { title = "Search Google", fn = searchGoogle },
    }
  },
  { title = "-" },
  { title = "New Mail Message", fn = newMailMessage },
  { title = "Copy Mail Message URL", fn = copyMailURL},
  { title = "-" },
  { title = "Paste Plain Text", fn = pastePlainText},
  { title = "New Finder Window", fn = newFinderWindow},
}

hammerMenuSet()



-- User Keymaps
-- ----------------------------------------------
-- This creates keymaps for specific apps, and creates an application watcher
-- that activates and deactivates the mappings when the associated app
-- activates.

-- The Apps you want to customize
readlineModeMap = hs.hotkey.modal.new()

-- Readline
readlineModeMap:bind({'alt'}, 'l', function() menuLowerCase() end)
readlineModeMap:bind({'alt'}, 'c', function() menuCapitalize() end)
readlineModeMap:bind({'alt'}, 'u', function() menuUpperCase() end)
readlineModeMap:bind({'alt'}, 'b', function() keyUpDown({'alt'}, 'left') end)
readlineModeMap:bind({'alt'}, 'f', function() keyUpDown({'alt'}, 'right') end)
readlineModeMap:bind({'alt'}, 'd', function() keyUpDown({'alt'}, 'forwarddelete') end)

-- App Activation Watcher
function appActivation(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      if (appName == "Emacs") or (appName == "Terminal") then
         readlineModeMap:exit()
      else
         readlineModeMap:enter()
      end
   end
end

appActivationWatcher = hs.application.watcher.new(appActivation)
appActivationWatcher:start()


-- Transient Keymap
-- ---------------------------------------------
-- This creates a custom transient keymap that is only active for one event
-- and then exists.

-- Requires a table named 'transientKeysBindings'

-- Create the model keymap to bind inside of
transientKeys = hs.hotkey.modal.new()

function transientKeys:entered()
  -- Create the menubar item
  myKeysMenuItem = hs.menubar.new():setTitle("􀇳 Transient Keymap!")
  myKeysMenuItem:setTooltip("Press Escape to deactivate.")
end

function transientKeys:exited()
  -- Remove the menu item
  myKeysMenuItem:delete()
end

-- Keymap should provide an escape and prevent recursion.
do
   local mod = {'cmd'}
   local key = 'e'
   hs.hotkey.bind(mod, key, function() transientKeys:enter() end)
   transientKeys:bind(mod, key, function() transientKeys:exit() end)
   transientKeys:bind('', 'escape', function() transientKeys:exit() end)
end

-- Set the kindings in keymap
for i, mapping in ipairs(transientKeysBindings) do
  local mod = mapping[1]
  local key = mapping[2]
  local fn  = mapping[3]
  transientKeys:bind(mod, key, function()
  if (type(fn) == 'string') then
    hs.application.launchOrFocus(fn)
  else
    fn()
  end
  transientKeys:exit() end)
end


-- Bindings
-- ---------------------------------------------
-- This binds keys to functions in the 'keyBindings' list.

for i, mapping in ipairs(keyBindings) do
   local mod = mapping[1]
   local key = mapping[2]
   local fn  = mapping[3]
   hs.hotkey.bind(mod, key, function() fn() end)
end


-- END HAMMERSPOON CONFIG --
