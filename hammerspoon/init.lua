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
local hyper = {"ctrl", "alt", "cmd"}
local omega = {"ctrl", "cmd"}

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

require('bluetooth_sleep')
require('keybinds') -- mostly for remapping keys


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

-- Resize and Move Windows
hs.hotkey.bind(hyper, 'left', function() hs.window.focusedWindow():moveToUnit({0, 0, 1/2, 1}) end)
hs.hotkey.bind(hyper, 'right', function() hs.window.focusedWindow():moveToUnit({1/2, 0, 1/2, 1}) end)
hs.hotkey.bind(hyper, 'h', function() hs.window.focusedWindow():moveToUnit({0, 0, 1/3+0.01, 1}) end)
hs.hotkey.bind(hyper, 'j', function() hs.window.focusedWindow():moveToUnit({0, 0, 2/3-0.01, 1}) end)
hs.hotkey.bind(hyper, 'k', function() hs.window.focusedWindow():moveToUnit({1/3+0.01, 0, 2/3-0.01, 1}) end)
hs.hotkey.bind(hyper, 'l', function() hs.window.focusedWindow():moveToUnit({2/3-0.01, 0, 1/3+0.01, 1}) end)
hs.hotkey.bind(hyper, 'f', function() hs.window.focusedWindow():moveToUnit({0, 0, 1, 1}) end)
hs.hotkey.bind(hyper, 'return', function() hs.window.focusedWindow():moveToUnit({1/24, 1/24, 5/12, 10/12}) end)

-- window hints
hs.hotkey.bind(hyper, 'i', hs.hints.windowHints)

-- window grid
hs.grid.setGrid('6x4', nil, nil)
hs.grid.setMargins({0, 0})
hs.hotkey.bind(hyper, ';', hs.grid.show)


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
   g = 'Mimestream',
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

hs.hotkey.bind({'ctrl', 'cmd'}, "h", function() os.execute( "open https://news.ycombinator.com" ) end)

if (hostname == "shadowfax") then
   hs.hotkey.bind({'ctrl', 'cmd'}, "k", function()
	 os.execute( "open https://ievfx.slack.com" )
   end)
else
   hs.hotkey.bind({'ctrl', 'cmd'}, "k", function()
	 hs.application.launchOrFocus("Slack")
   end)
end


-- Google File Stream Chooser
-- -----------------------------------------------
-- Hacked together from here: https://github.com/ebai101/dotfiles/blob/master/config/hammerspoon/reason.lua

local fileStreamChooser = hs.chooser.new(function(choice) hs.open(choice['subText']) end)

local function fileStreamPopulate()
   local options = {}
   hs.task.new('/usr/bin/find', function(task, out, err)
		  for path in out:gmatch("[^\r\n]+") do
		     table.insert(options, {
				     ['text'] = string.match(path, ".*/(.+)$"),
				     ['subText'] = path,
				     ['modified'] = hs.fs.attributes(path).modification
		     })
		  end

		  table.sort(options, function(a, b)
				return a['modified'] > b['modified']
		  end)

		  fileStreamChooser:choices(options)

   end, { '/Volumes/GoogleDrive/Shared drives', '-type', 'd', '-maxdepth', '1' }):start()
   -- currently limited to a 'maxdepth' of 1 due to: https://github.com/Hammerspoon/hammerspoon/issues/2651
end

fileStreamChooser:showCallback(fileStreamPopulate)
fileStreamChooser:searchSubText(true)

hs.hotkey.bind( hyper, "o", function() fileStreamChooser:show() end)

-- QuickMenu
-- -----------------------------------------------
-- This is used for misc scripts I want to occasionally run, but don't want to
-- bother remembering a keybinding for. Somewhat like my personal FastScripts.

-- Create menu bar item
local quickMenu = hs.menubar.new()

function buildQuickMenu()
   local snippetMenu = {
      { title = "waving hands around", fn = snipWave },
      { title = " ¯\\_(ツ)_/¯", fn = snipShrug },
      { title = "[Org Mode Date]", fn = snipOrgDate },
      { title = "[YYYY-MM-DD]", fn = snipISODate },
   }
   local menuTable = {
      { title = "QuickMenu", disabled = true },
      { title = "-" },
      { title = "Copy Mail Message URL", fn = copyMailURL, shortcut = "m"},
      { title = "New Mail Message", fn = newMailMessage },
      { title = "-" },
      { title = "Open Org Inbox", fn = openOrgInbox, shortcut = "i" },
      { title = "-" },
      { title = "Safari tabs → Org Inbox", fn = safariTabs2ORG },
      { title = "iOS Inbox → Org Inbox", fn = importIOSinbox },
      { title = "Clipboard → Org Inbox", fn = clipboard2ORG },
      { title = "-" },
      { title = "Snippets", menu = snippetMenu },
      { title = "Open Google Drive Folder", fn = function() fileStreamChooser:show() end },
      { title = "-" },
      { title = "Remove From Menu Bar", fn = killQuickMenu },
   }
   local utilIcon = hs.image.imageFromPath("assets/hare.pdf")
   quickMenu:setIcon(utilIcon:setSize({w=20,h=20}))
   quickMenu:setMenu(menuTable)
end


-- FUNCTIONS FOR MENUBAR --

function killQuickMenu() quickMenu:delete() end

function safariTabs2ORG()
   os.execute( "printf '\n** TODO Safari Tabs\n\n' >> ~/home/org/inbox.org && ~/home/dot/bin/safariTabs >> ~/home/org/inbox.org && open hammerspoon://success" )
end

function clipboard2ORG()
   os.execute( "printf '\n** TODO Clipboard Refile\n\n' >> ~/home/org/inbox.org && pbpaste >> ~/home/org/inbox.org && open hammerspoon://success" )
end

function copyMailURL()
   os.execute( "~/home/dot/bin/getMailURL | pbcopy | open hammerspoon://success" )
end

function openOrgInbox() os.execute("open ~/home/org/inbox.org") end
function importIOSinbox() os.execute("~/Desktop/moveiOS2ORG") end

function newMailMessage() os.execute("open mailto:") end
hs.hotkey.bind({"ctrl", "cmd", "shift"}, "m", newMailMessage)

-- SNIPPETS --
function snipWave() hs.eventtap.keyStrokes("(waving hands around)") end
function snipShrug() hs.eventtap.keyStrokes(" ¯\\_(ツ)_/¯") end
function snipOrgDate() hs.eventtap.keyStrokes(os.date("<%Y-%m-%d %a>")) end
function snipISODate() hs.eventtap.keyStrokes(os.date("%Y-%m-%d")) end

-- Finally, build the menubar item
buildQuickMenu()


-- Backup Menu
-- -----------------------------------------------
-- A menu item for monitoring/controlling restic backups run by launchd.
-- The actual backup script interacts with this.

-- Create the menubar item
local backupMenu = hs.menubar.new()
-- Set the default icon, this will be replaced when the backup job succeeds/fails
local cloud_idle = hs.image.imageFromPath("assets/cloud_idle.pdf")
backupMenu:setIcon(cloud_idle:setSize({w=20,h=20}))

-- functions called by the menubar item
function backupNow () os.execute("launchctl start local.restic.test") end
function backupOpenLogs () os.execute("open ~/home/opt/restic/logs") end
function backupRunning()
   local cloud_run = hs.image.imageFromPath("assets/cloud_run.pdf")
   backupMenu:setIcon(cloud_run:setSize({w=20,h=20}))
end
function backupSuccess()
   local cloud_ok = hs.image.imageFromPath("assets/cloud_ok.pdf")
   backupMenu:setIcon(cloud_ok:setSize({w=20,h=20}))
end
function backupFail()
   local cloud_fail = hs.image.imageFromPath("assets/cloud_fail.pdf")
   backupMenu:setIcon(cloud_fail:setSize({w=20,h=20}))
end

-- Register URLs and bind them to the above functions so that the backup
-- script can update the menu item.
hs.urlevent.bind("backup_running", backupRunning)
hs.urlevent.bind("backup_success", backupSuccess)
hs.urlevent.bind("backup_fail", backupFail)

-- Build the menu item
function backupMenuItem()
   -- First, get the LAST log file, we will use this in the menuTable
   local lastBackup = hs.execute("ls ~/home/opt/restic/logs | tail -1")
   -- Generate the menu items you want in the list
   local menuTable = {
      { title = "Last Backup:", disabled = true },
      { title = lastBackup, fn = backupOpenLogs },
      { title = "-" },
      { title = "Backup Now", fn = backupNow },
   }
   backupMenu:setMenu(menuTable)
end

-- Run the function to place an item in the menubar
backupMenuItem()


-- Reload Notification in Menubar
-- -----------------------------------------------
-- This places a temporary message in the menubar. Because it comes and goes,
-- it should probably be last in your config.

local HSNotifyMenu = hs.menubar.new()

function buildHSNotifyMenu()
-- When called, places the message in the menubar
-- and after a delay, calls a function to remove this item from the menubar
   HSNotifyMenu:setTitle("🤘 Reloaded!")
   hs.timer.doAfter(3, killHSNotifyMenu)
end

function killHSNotifyMenu()
   HSNotifyMenu:delete()
end

-- Show when this file is evaluated
buildHSNotifyMenu()



-- END --

