-- Oliver Taylor's Hammerspoon Config
-- https://github.com/overvale/dotfiles


-- Setup
-- -----------------------------------------------

local hyper = {'cmd', 'alt', 'ctrl'}

hs.window.animationDuration = 0

function keyUpDown(modifiers, key)
  hs.eventtap.keyStroke(modifiers, key, 0)
end

require("macos-toggles")
require("functions")

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

function keyBindingsSet ()
  for i, mapping in ipairs(keyBindings) do
    local mod = mapping[1]
    local key = mapping[2]
    local fn  = mapping[3]
    hs.hotkey.bind(mod, key, function() fn() end)
  end
end


-- Transient
-- ---------------------------------------------

require("transient")

-- Accepts strings and function names
-- Strings are assumed to be Application names
transientBindings = {
  { {}, 'm', 'Mail' },
  { {}, 'c', 'Calendar' },
  { {}, 'b', 'BBEdit' },
  { {}, 'n', 'Notes'},
  { {}, 's', 'Safari' },
  { {}, 'a', 'Music' },
  { {}, 't', 'Terminal' },
  { {}, 'r', 'Reminders' },
  { {}, 'f', newFinderWindow },
  { {}, 'g', searchGoogle },
  { {}, 'd', todoList },
}

transientSetBindings()


-- Hammer Menu
-- ---------------------------------------

hammerMenuTable = {
  { title = "Toggle Dark Mode", fn = toggleDarkMode },
  { title = "Toggle Stage Manager", fn = toggleStageMan },
  { title = "Toggle Menu Bar", fn = toggleMenubar },
  { title = "-" },
  { title = "Todo List", fn = todoList},
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

hammerMenu = hs.menubar.new()

styledTitle = hs.styledtext.new("􀂢", {
    font = { size = 16.5 },
})

hammerMenu:setTitle(styledTitle)

hammerMenu:setMenu(hammerMenuTable)


-- Backup Menu
-- --------------------------------------------------

backupMenu = hs.menubar.new()

function lastBackupNAS ()
   local output, _, _ = hs.execute("/Users/oht/home/src/rsync-backup/last-backup-NAS.sh")
   return output
end

function lastBackupCloud ()
   local output, _, _ = hs.execute("/Users/oht/home/src/rsync-backup/last-backup-cloud.sh")
   return output
end

function backupMenuItem()
    local menuTable = {
      { title = "Latest Backups:", disabled = true },
      { title = lastBackupNAS(), disabled = true },
      { title = lastBackupCloud(), disabled = true },
    }
    return menuTable
end

backupMenu:setMenu(backupMenuItem)

backupMenu:setTitle("􀙖")


-- App-Specific Keymaps
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


-- END HAMMERSPOON CONFIG --
