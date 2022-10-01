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

local hyper = {'cmd', 'alt', 'ctrl'}
local alpha = {'cmd', 'ctrl'}

anycomplete = hs.loadSpoon("Anycomplete")
anycomplete.engine = "duckduckgo"
anycomplete.bindHotkeys()

hs.loadSpoon("ClipboardTool")
spoon.ClipboardTool:bindHotkeys( { show_clipboard = {hyper, "v"} })
spoon.ClipboardTool.paste_on_select = true
spoon.ClipboardTool.show_in_menubar = false
spoon.ClipboardTool.show_copied_alert = false
spoon.ClipboardTool:start()

hs.window.animationDuration = 0


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

function openWorkApps() os.execute( "open -a Dropbox; open -a OneDrive; open -a Tailscale" ) end
function killWorkApps() os.execute( "killall {Dropbox,Tailscale,OneDrive}" ) end

function toggleMute()
   local teams = hs.application.find("com.microsoft.teams")
   hs.eventtap.keyStroke({"cmd","shift"}, "m", 0, teams)
end

hs.hotkey.bind(hyper, "1", toggleMute)


-- Sky Rocket Spoon -- Window Resizing with Mouse
-- -----------------------------------------------

local SkyRocket = hs.loadSpoon("SkyRocket")

sky = SkyRocket:new({
  -- Opacity of resize canvas
  opacity = 0.25,

  -- Which mouse button to hold to move a window?
  moveMouseButton = 'left',
  -- Which modifiers to hold to move a window?
  moveModifiers = {'shift', 'ctrl'},

  -- Which mouse button to hold to resize a window?
  resizeMouseButton = 'left',
  -- Which modifiers to hold to resize a window?
  resizeModifiers = {'cmd', 'alt'},
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
      { title = "<YYYY-MM-DD Day>", fn = snipOrgDate },
      { title = "YYYY-MM-DD", fn = snipISODate },
   }
   local menuTable = {
      { title = "Hammerspoon", disabled = true },
      { title = "-" },
      { title = "Dark/Light Mode", fn = toggleDarkMode },
      { title = "Snippets", menu = snippetMenu },
      { title = "Open BBEdit Scratchpad", fn = bbeditScratch },
      { title = "-" },
      { title = "Launch Emacs Debug Init", fn = emacsDebugInit },
      { title = "Launch Emacs Q", fn = emacsQ },
      { title = "-" },
      { title = "Copy Mail Message URL", fn = copyMailURL},
      { title = "New Mail Message", fn = newMailMessage },
      { title = "-" },
      { title = "Outpost", disabled = true },
      { title = "Open Work Apps", fn = openWorkApps },
      { title = "Close Work Apps", fn = killWorkApps },
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

hs.loadSpoon("MiroWindowsManager")
spoon.MiroWindowsManager:bindHotkeys({
	up         = {hyper, "up"},
	down       = {hyper, "down"},
	left       = {hyper, "left"},
	right      = {hyper, "right"},
	fullscreen = {hyper, "f"},
	nextscreen = {hyper, "n"}
})

-- App Launcher
-- -----------------------------------------------

local applicationHotkeys = {
   m = 'Mail',
   c = 'Calendar',
   b = 'BBEdit',
   s = 'Safari',
   a = 'Music',
   u = 'Terminal',
   r = 'Reminders',
}

for key, app in pairs(applicationHotkeys) do
   hs.hotkey.bind(alpha, key, function()
                     hs.application.launchOrFocus(app)
   end)
end




-- Reload Notification
-- ----------------------------------------------

-- When this config is loaded, or reloaded, notify that it was done
-- successfully.
hs.notify.new({title='Hammerspoon', informativeText='Ready to Rock! 🤘'}):send()


-- END HAMMERSPOON CONFIG --
