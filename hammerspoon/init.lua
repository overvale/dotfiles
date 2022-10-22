-- Oliver Taylor's Hammerspoon Config
-- homepage: https://github.com/olivertaylor/dotfiles

-- Acceptable keycodes are here:
-- https://www.hammerspoon.org/docs/hs.keycodes.html#map

-- Inspiration:
-- https://spinscale.de/posts/2016-11-08-creating-a-productive-osx-environment-hammerspoon.html
-- https://medium.com/@robhowlett/hammerspoon-the-best-mac-software-youve-never-heard-of-40c2df6db0f8
-- https://github.com/jasonrudolph/keyboard
-- https://github.com/dbmrq/dotfiles
-- https://github.com/raulchen/dotfiles
-- https://github.com/justintanner/universal-emacs-keybindings
-- https://github.com/dbalatero/dotfiles/tree/master/hammerspoon
-- https://github.com/senorprogrammer/hammerspoon_init/

-- Setup
-- -----------------------------------------------

local hyper = {'cmd', 'alt', 'ctrl'}
local alpha = {'cmd', 'ctrl'}

hs.window.animationDuration = 0


-- Utility Functions
-- -----------------------------------------------

local function appTitle()
   -- Return title of foreground app
   app = hs.application.frontmostApplication()
   if app ~= nil then
      return app:title()
   end
end

function notify(title, text)
  hs.notify.new({ title = title, informativeText = text}):send()
end

function genericSuccess()
   -- Function for creating a notification saying "Success!"
   -- This is useful when testing new Hammerspoon stuff.
   notify('Hammerspoon', 'Success!')
end

-- With the below you can run `open hammerspoon:///success` in your terminal
-- and you'll run the 'genericSuccess' function.
hs.urlevent.bind("success", genericSuccess)

function pastePlainText()
  local paste = hs.pasteboard.readString()
  hs.pasteboard.setContents(paste)
  app = hs.application.frontmostApplication()
  app:selectMenuItem({"Edit", "Paste"})
end


-- Misc Functions
-- ------------------------------------------------

function bbeditScratch()
   os.execute( "osascript -e 'tell application \"BBEdit\" to (open scratchpad document) activate'" )
end

function backupOpenLogs () os.execute("open ~/home/src/rsync-backup/logs") end
function rsyncBackup() os.execute( "~/home/src/rsync-backup/laptop-backup.sh" ) end

function emacsDebugInit() os.execute( "~/Applications/Emacs.app/Contents/MacOS/Emacs --debug-init" ) end
function emacsQ() os.execute( "~/Applications/Emacs.app/Contents/MacOS/Emacs -q" ) end

function copyMailURL() os.execute( "~/home/dot/bin/getMailURL | pbcopy | open hammerspoon://success" ) end
function newMailMessage() os.execute("open mailto:") end

function snipWave() hs.eventtap.keyStrokes("(waving hands around)") end
function snipShrug() hs.eventtap.keyStrokes(" ¯\\_(ツ)_/¯") end
function snipOrgDate() hs.eventtap.keyStrokes(os.date("<%Y-%m-%d %a>")) end
function snipISODate() hs.eventtap.keyStrokes(os.date("%Y-%m-%d")) end

-- Rather than switch to Safari, copy the current URL, switch back to the previous app and paste,
-- This is a function that fetches the current URL from Safari and types it
function typeCurrentSafariURL()
    script = [[
    tell application "Safari"
        set currentURL to URL of document 1
    end tell
    return currentURL
    ]]
    ok, result = hs.applescript(script)
    if (ok) then
        hs.eventtap.keyStrokes(result)
    end
end

function searchYouTube()
   button, message = hs.dialog.textPrompt("Youtube", "Search YouTube for:", "", "Search", "Cancel")
   if button == 'Cancel' then
      return
   else
      search = hs.http.encodeForQuery(message)
      os.execute("open http://www.youtube.com/results?search_query=" .. search)
   end
end

function searchGitHub()
   button, message = hs.dialog.textPrompt("GitHub", "Search GitHub for:", "", "Search", "Cancel")
   if button == 'Cancel' then
      return
   else
      search = hs.http.encodeForQuery(message)
      os.execute("open https://github.com/search?q=" .. search)
   end
end

function searchWikipedia()
   button, message = hs.dialog.textPrompt("Wikipedia", "Search Wikipedia for:", "", "Search", "Cancel")
   if button == 'Cancel' then
      return
   else
      search = hs.http.encodeForQuery(message)
      os.execute("open https://en.wikipedia.org/w/index.php?search=" .. search)
   end
end

function searchIMDB()
   button, message = hs.dialog.textPrompt("IMDB", "Search IMDB for:", "", "Search", "Cancel")
   if button == 'Cancel' then
      return
   else
      search = hs.http.encodeForQuery(message)
      os.execute("open https://www.imdb.com/find?q=" .. search)
   end
end


-- MacOS System Stuff
-- ----------------------------------------------

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

function toggleMenubar()
   hs.applescript([[
tell application "System Events"
    tell dock preferences to set autohide menu bar to not autohide menu bar
end tell]])
end


-- Private
-- ----------------------------------------------

require("private")


-- Readline Keymap
-- ----------------------------------------------
-- MacOS supports lots of Emacs-style shortcuts out of the box, but it is
-- missing M-f, M-b, M-d -- and I think it should also support the readline
-- shortcuts  C-u, C-w.
-- https://readline.kablamo.org/emacs.html
-- However, rather than just binding them globally, I want to switch them off
-- when Emacs and the Terminal are the foreground app, so the below code does
-- all that.

-- First, create the keymap you'd like to bind into.
local ReadlineKeymap = hs.hotkey.modal.new()

-- Next, create the functions you want to use...
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

-- Then bind some keys to the map.
ReadlineKeymap:bind({'ctrl'}, 'w', deleteWordBack)
ReadlineKeymap:bind({'ctrl'}, 'u', deleteLineBack)
ReadlineKeymap:bind({'alt'},  'd', deleteWordForward)
ReadlineKeymap:bind({'alt'},  'b', moveWordBack)
ReadlineKeymap:bind({'alt'},  'f', moveWordForward)


-- Activate App-Specific Keymaps
-- ------------------------------------------------

local function setUserKeymaps()
      if appTitle() == "Emacs" or appTitle() == "Terminal" then
         -- print('Readline keymap exited.')
         ReadlineKeymap:exit()
      else
         -- print('Readline keymap entered.')
         ReadlineKeymap:enter()
      end
end

local function appWatcherFunction(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      setUserKeymaps()
      if (appName == "Finder") then
         -- Bring all Finder windows forward when one gets activated
         appObject:selectMenuItem({"Window", "Bring All to Front"})
      end
   end
end

setUserKeymaps()
print('User Keymaps activated.')

local appWatcher = hs.application.watcher.new(appWatcherFunction)

appWatcher:start()
print('Application Watcher started.')


-- My Hammerspoon Menubar Item
-- ----------------------------------------------

-- Name the menubar item
local myHammerMenu = hs.menubar.new()

-- Build the actual menubar item drop-down
function myHammerMenuItem()
   local snippetMenu = {
      { title = "waving hands around", fn = snipWave },
      { title = " ¯\\_(ツ)_/¯", fn = snipShrug },
      { title = "<YYYY-MM-DD Day>", fn = snipOrgDate },
      { title = "YYYY-MM-DD", fn = snipISODate },
   }
   local searchMenu = {
      { title = "Search YouTube", fn = searchYouTube },
      { title = "Search GitHub", fn = searchGitHub },
      { title = "Search Wikipedia", fn = searchWikipedia },
      { title = "Search IMDB", fn = searchIMDB },
   }
   local menuTable = {
      { title = "Dark/Light Mode", fn = toggleDarkMode },
      { title = "Open BBEdit Scratchpad", fn = bbeditScratch },
      { title = "Open Console", fn = hs.openConsole},
      { title = "Search", menu = searchMenu },
      { title = "-" },
      { title = "Type Safari URL", fn = typeCurrentSafariURL},
      { title = "Paste as Plain Text", fn = pastePlainText },
      { title = "Snippets", menu = snippetMenu },
      { title = "-" },
      { title = "Outpost", disabled = true },
      { title = "Note to Self", fn = noteToWorkSelf },
      { title = "Open Work Apps", fn = openWorkApps },
      { title = "Close Work Apps", fn = killWorkApps },
      { title = "Meeting Times", fn = meetingTimes },
      { title = "Open from Dropbox...", fn = openDropboxProject },
      { title = "New Pipeline Task", fn = newPipelineTask},
      { title = "-" },
      { title = "Mail", disabled = true},
      { title = "Copy Mail Message URL", fn = copyMailURL},
      { title = "New Mail Message", fn = newMailMessage },
      { title = "-" },
      { title = "Rsync Backups", disabled = true },
      { title = "Rsync to NAS", fn = rsyncBackup },
      { title = "Open Logs", fn = backupOpenLogs },
   }
   myHammerMenu:setMenu(menuTable)
end

-- Add the menubar item to the menubar
myHammerMenuItem()

local iconH = [[ASCII:
1.............1
7.............5
...............
...............
....AD...FI....
...............
...............
....K.....L....
....N.....M....
...............
...............
....BC...GH....
...............
...............
7.............5
3.............3
]]

myHammerMenu:setIcon(iconH)


-- Window Moving
-- -----------------------------------------------

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

function wm_left()        moveWindow('left') end
function wm_right()       moveWindow('right') end
function wm_center()      hs.window.focusedWindow():centerOnScreen() end


-- Misc Spoons
-- -----------------------------------------------

anycomplete = hs.loadSpoon("Anycomplete")
anycomplete.engine = "duckduckgo"
anycomplete.bindHotkeys()

hs.loadSpoon("MiroWindowsManager")
spoon.MiroWindowsManager:bindHotkeys({
	up         = {hyper, "up"},
	down       = {hyper, "down"},
	left       = {hyper, "left"},
	right      = {hyper, "right"},
	fullscreen = {hyper, "f"},
	nextscreen = {hyper, "n"}
})


-- Bindings
-- ----------------------------------------------

local applicationHotkeys = {
   m = 'Mail',
   c = 'Calendar',
   b = 'BBEdit',
   e = 'Emacs',
   s = 'Safari',
   a = 'Music',
   t = 'Terminal',
   r = 'Reminders',
}

local alphaHotkeys = {
   h = reloadHammerspoon,
   p = bbeditScratch,
   o = openDropboxProject,
}

local hyperHotkeys = {
   t = snipISODate,
   d = toggleDarkMode,
   j = wm_left,
   k = wm_center,
   l = wm_right,
}

for key, fn  in pairs(alphaHotkeys) do hs.hotkey.bind(alpha, key, fn) end
for key, fn  in pairs(hyperHotkeys) do hs.hotkey.bind(hyper, key, fn) end
for key, app in pairs(applicationHotkeys) do
   hs.hotkey.bind(alpha, key, function() hs.application.launchOrFocus(app) end)
end

-- Similar to binding to hide/show the Dock.
hs.hotkey.bind({'alt', 'cmd'}, 'm', toggleMenubar)


-- Mail Keys
-- ---------------------------------------------
-- This creates a keymap you can activate/deactivate on demand,
-- bind keys to, and it displays a status message in the menubar.

-- Build a keymap to bind into
mailKeys = hs.hotkey.modal.new()

function mailKeys:entered()
   mailKeysMenuItem = hs.menubar.new():setTitle("Mail Keys Mode")
   mailKeysMenuItem:setTooltip("Press Escape to deactivate.")
end

function mailKeys:exited()
   mailKeysMenuItem:delete()
end

mailKeys:bind('', 'escape', function() mailKeys:exit() end)
mailKeys:bind(hyper, 'm', function() mailKeys:exit() end)
mailKeys:bind('', 'n', function() newMailMessage() mailKeys:exit() end)
mailKeys:bind('', 'l', function() noteToWorkSelf() mailKeys:exit() end)

hs.hotkey.bind(hyper, 'm', function() mailKeys:enter() end)


-- Reload Config
-- ----------------------------------------------

local reloadHammerspoon = function() hs.reload() end

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

reloadWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/home/dot/hammerspoon/", reloadConfig):start()

-- When this config is loaded, or reloaded, send notification.
notify("Hammerspoon", "Config reloaded, ready to rock! 🤘")


-- END HAMMERSPOON CONFIG --
