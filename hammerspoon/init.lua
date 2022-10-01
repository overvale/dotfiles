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

-- Setup
-- -----------------------------------------------

local hyper = {'cmd', 'alt', 'ctrl'}
local alpha = {'cmd', 'ctrl'}

hs.window.animationDuration = 0


-- Misc Functions
-- -----------------------------------------------

local function appTitle()
   -- Return title of foreground app
   app = hs.application.frontmostApplication()
   if app ~= nil then
      return app:title()
   end
end

function genericSuccess()
   -- Function for creating a notification saying "Success!"
   -- This is useful when testing new Hammerspoon stuff.
   hs.notify.new({title='Hammerspoon', informativeText='Success!'}):send()
end

-- With the below you can run `open hammerspoon:///success` in your terminal
-- and you'll run the 'genericSuccess' function.
hs.urlevent.bind("success", genericSuccess)

local reloadHammerspoon = function() hs.reload() end

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
function snipCircle() hs.eventtap.keyStrokes(os.date("○")) end

function openWorkApps() os.execute( "open -a Dropbox; open -a OneDrive; open -a Tailscale" ) end
function killWorkApps() os.execute( "killall {Dropbox,Tailscale,OneDrive}" ) end

function noteToWorkSelf()
  os.execute( "open mailto:otaylor@outpost-vfx.com" )
end

function toggleTeamsMute()
   local teams = hs.application.find("com.microsoft.teams")
   hs.eventtap.keyStroke({"cmd","shift"}, "m", 0, teams)
end

function meetingTimes()
  os.execute( "/Users/oht/home/dot/bin/meet | open -tf" )
end

function miniCalendar()
  os.execute( "cal -h | open -tf" )
end

function pastePlainText()
  hs.execute([[
    osascript -e 'the clipboard as «class RTF »' | \
        perl -ne 'print chr foreach unpack("C*",pack("H*",substr($_,11,-3)))' | \
        textutil -stdin -stdout -convert txt
]]) end


-- Toggle Dark Mode
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

local ReadlineKeymap = hs.hotkey.modal.new()

-- Bind some keys to it
ReadlineKeymap:bind({'ctrl'}, 'w', deleteWordBack)
ReadlineKeymap:bind({'ctrl'}, 'u', deleteLineBack)
ReadlineKeymap:bind({'alt'},  'd', deleteWordForward)
ReadlineKeymap:bind({'alt'},  'b', moveWordBack)
ReadlineKeymap:bind({'alt'},  'f', moveWordForward)



-- Readline Hotkeys
-- ------------------------------------------------

local function readlineWatcherFunction(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      if appTitle() == "Emacs" or appTitle() == "Terminal" then
         print('Readline keymap exited.')
         ReadlineKeymap:exit()
      else
         print('Readline keymap entered.')
         ReadlineKeymap:enter()
      end
   end
end

local readlineWatcher = hs.application.watcher.new(readlineWatcherFunction)

readlineWatcher:start()

readlineWatcherFunction()


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
   local menuTable = {
      { title = "Dark/Light Mode", fn = toggleDarkMode },
      { title = "Open BBEdit Scratchpad", fn = bbeditScratch },
      { title = "Paste as Plain Text", fn = pastePlainText },
      { title = "-" },
      { title = "Snippets", menu = snippetMenu },
      { title = "-" },
      { title = "Outpost", disabled = true },
      { title = "Note to Self", fn = noteToWorkSelf },
      { title = "Open Work Apps", fn = openWorkApps },
      { title = "Close Work Apps", fn = killWorkApps },
      { title = "Meeting Times", fn = meetingTimes },
      { title = "Mini Calendar", fn = miniCalendar },
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


-- Misc Spoons
-- -----------------------------------------------

anycomplete = hs.loadSpoon("Anycomplete")
anycomplete.engine = "duckduckgo"
anycomplete.bindHotkeys()

hs.loadSpoon("ClipboardTool")
spoon.ClipboardTool:bindHotkeys( { show_clipboard = {alpha, "v"} })
spoon.ClipboardTool.paste_on_select = true
spoon.ClipboardTool.show_in_menubar = false
spoon.ClipboardTool.show_copied_alert = false
spoon.ClipboardTool:start()

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
   s = 'Safari',
   a = 'Music',
   u = 'Terminal',
   r = 'Reminders',
}

local alphaHotkeys = {
   h = reloadHammerspoon,
   p = bbeditScratch,
   n = noteToWorkSelf,
}

local hyperHotkeys = {
   t = snipISODate,
   m = toggleTeamsMute,
   d = toggleDarkMode,
}

hs.hotkey.bind({'alt', 'shift'}, '0', snipCircle)

for key, fn  in pairs(alphaHotkeys) do hs.hotkey.bind(alpha, key, fn) end
for key, fn  in pairs(hyperHotkeys) do hs.hotkey.bind(hyper, key, fn) end
for key, app in pairs(applicationHotkeys) do
   hs.hotkey.bind(alpha, key, function() hs.application.launchOrFocus(app) end)
end


-- Reload Notification
-- ----------------------------------------------

-- When this config is loaded, or reloaded, notify that it was done
-- successfully.
hs.notify.new({title='Hammerspoon', informativeText='Ready to Rock! 🤘'}):send()


-- END HAMMERSPOON CONFIG --
