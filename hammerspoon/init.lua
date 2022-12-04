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
local power = {'cmd', 'shift'}

hs.window.animationDuration = 0

-- Personal namespace
oht = {}


-- Utility Functions
-- -----------------------------------------------

function oht.execute(x)
   -- Executes the argument (string) as a function.
   _G[x]()
end

function oht.appTitle()
   -- Return title of foreground app
   app = hs.application.frontmostApplication()
   if app ~= nil then
      return app:title()
   end
end

function oht.notify(title, text)
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


-- Misc Functions
-- ------------------------------------------------

function editHammerspoonInit()
   os.execute( "open -a Emacs ~/home/dot/hammerspoon/init.lua" )
end

function pastePlainText()
   p = hs.pasteboard.readDataForUTI(nil, "public.utf8-plain-text")
   hs.pasteboard.setContents(p)
   app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Paste"})
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

function webSearch(name, url)
   hs.focus()
   button, message = hs.dialog.textPrompt(name, "Search " .. name .. "for:", "", "Search", "Cancel")
   if button == 'Cancel' then
      return
   else
      search = hs.http.encodeForQuery(message)
      os.execute("open " .. url .. search)
   end
end

function searchYouTube()   webSearch("YouTube",   "http://www.youtube.com/results?search_query=") end
function searchGitHub()    webSearch("GitHub",    "https://github.com/search?q=") end
function searchWikipedia() webSearch("Wikipedia", "https://en.wikipedia.org/w/index.php?search=") end
function searchIMDB()      webSearch("IMDB",      "https://www.imdb.com/find?q=") end

function chooseMenuItem()
   -- from: https://github.com/brokensandals/MenuChooser.spoon
   local app = hs.application.frontmostApplication()
   app:getMenuItems(function(menu)
         local choices = {}
         function findChoices(pathstr, path, list)
            for _,item in pairs(list) do
               local newpathstr
               if pathstr then
                  newpathstr = pathstr .. ' › ' .. (item.AXTitle or '')
               else
                  newpathstr = item.AXTitle
               end
               local newpath = {}
               for i,title in ipairs(path) do
                  newpath[i] = title
               end
               newpath[#newpath+1] = item.AXTitle
               if item.AXChildren then
                  findChoices(newpathstr, newpath, item.AXChildren[1])
               elseif item.AXEnabled and item.AXTitle and (not (item.AXTitle == '')) then
                  choices[#choices+1] = {
                     text = newpathstr,
                     path = newpath
                  }
               end
            end
         end
         findChoices(nil, {}, menu)
         local chooser = hs.chooser.new(function(selected)
               if selected then
                  app:selectMenuItem(selected.path)
               end
         end)
         chooser:choices(choices)
         chooser:placeholderText('Menu Item')
         chooser:bgDark(true)
         chooser:show()
   end)
end

function oht.newFinderWindow()
   finder = hs.appfinder.appFromName("Finder")
   hs.osascript.applescript('tell application "Finder" to make new Finder window')
   finder:activate()
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

pvt = require("private")


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
      if oht.appTitle() == "Emacs" or oht.appTitle() == "Terminal" then
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
      if (appName == "Microsoft Teams") then
         appObject:selectMenuItem({"Window", "Bring All to Front"})
      end
   end
end

setUserKeymaps()
print('User Keymaps activated.')

local appWatcher = hs.application.watcher.new(appWatcherFunction)

appWatcher:start()
print('Application Watcher started.')


-- M-x Anything
-- ----------------------------------------------

oht.mxchooser = hs.chooser.new(function(choice)
      if not choice then
         return
      else
         if choice["type"] == "app" then
            os.execute("open -a" .. choice["arg"])
         elseif choice["type"] == "path" then
            os.execute("open " .. choice["arg"])
         elseif choice["type"] == "func" then
            _ENV[choice["arg"]]()
         end
      end
end)

oht.mxchooser:choices({
      {["type"] = "path", ["text"] = "New Finder Window",                      ["arg"] = "newFinderWindow",},
      {["type"] = "path", ["text"] = "Desktop",                                ["arg"] = "~/Desktop",},
      {["type"] = "path", ["text"] = "Downloads",                              ["arg"] = "~/Downloads",},
      {["type"] = "path", ["text"] = "Home",                                   ["arg"] = "~/home",},
      {["type"] = "path", ["text"] = "iCloud Documents",                       ["arg"] = "~/Library/Mobile Documents/com~apple~CloudDocs/Oliver",},
      {["type"] = "app",  ["text"] = "Safari",                                 ["arg"] = "Safari",},
      {["type"] = "func", ["text"] = "Edit Hammerspoon Config",                ["arg"] = "editHammerspoonInit",},
      {["type"] = "func", ["text"] = "Open Rsync Backup Logs",                 ["arg"] = "backupOpenLogs",},
      {["type"] = "func", ["text"] = "Open BBEdit Scratch",                    ["arg"] = "bbeditScratch",},
      {["type"] = "func", ["text"] = "Choose Menu Item",                       ["arg"] = "chooseMenuItem",},
      {["type"] = "func", ["text"] = "Copy Mail Message URL",                  ["arg"] = "copyMailURL",},
      {["type"] = "func", ["text"] = "Kill Work Apps",                         ["arg"] = "killWorkApps",},
      {["type"] = "func", ["text"] = "Show Meeting Times",                     ["arg"] = "meetingTimes",},
      {["type"] = "func", ["text"] = "New Mail Message",                       ["arg"] = "newMailMessage",},
      {["type"] = "func", ["text"] = "Open Dropbox Bid folder",                ["arg"] = "pvt.openDropbox",},
      {["type"] = "func", ["text"] = "Open Work Apps",                         ["arg"] = "openWorkApps",},
      {["type"] = "func", ["text"] = "Paste as Plain Text",                    ["arg"] = "pastePlainText",},
      {["type"] = "func", ["text"] = "Reload Hammerspoon",                     ["arg"] = "reloadConfig",},
      {["type"] = "func", ["text"] = "Start Rsync Backup",                     ["arg"] = "rsyncBackup",},
      {["type"] = "func", ["text"] = "Search GitHub",                          ["arg"] = "searchGitHub",},
      {["type"] = "func", ["text"] = "Search IMDB",                            ["arg"] = "searchIMDB",},
      {["type"] = "func", ["text"] = "Search Wikipedia",                       ["arg"] = "searchWikipedia",},
      {["type"] = "func", ["text"] = "Search Youtube",                         ["arg"] = "searchYouTube",},
      {["type"] = "func", ["text"] = "Snippet: ISO Date",                      ["arg"] = "snipISODate",},
      {["type"] = "func", ["text"] = "Snippet: Org Mode Date",                 ["arg"] = "snipOrgDate",},
      {["type"] = "func", ["text"] = "Snippet ¯\\_(ツ)_/¯",                    ["arg"] = "snipShrug",},
      {["type"] = "func", ["text"] = "Snippet \"waving hands around\"",        ["arg"] = "snipWave",},
      {["type"] = "func", ["text"] = "Toggle Dark Mode",                       ["arg"] = "toggleDarkMode",},
      {["type"] = "func", ["text"] = "Toggle MenuBar",                         ["arg"] = "toggleMenubar",},
      {["type"] = "func", ["text"] = "Type Current Safari URL",                ["arg"] = "typeCurrentSafariURL",},
      {["type"] = "func", ["text"] = "Type Execs + MDs + EPs email addresses", ["arg"] = "typeExecMDsEPs",},
      {["type"] = "func", ["text"] = "Open Excel Scratch Doc",                 ["arg"] = "scratchExcel",},
})

oht.mxchooser:placeholderText("M-x Hammerspoon")
oht.mxchooser:bgDark(true)

hs.hotkey.bind(alpha, "x", function() oht.mxchooser:show() end)


-- Window Moving & Resizing
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

function wm_left()   moveWindow('left') end
function wm_right()  moveWindow('right') end
function wm_center() hs.window.focusedWindow():centerOnScreen() end

function winResizeLeft()
   if winPosition == 'left23' then
      winPosition = 'left12'
      hs.window.focusedWindow():moveToUnit({0, 0, 1/2, 1})
   elseif winPosition == 'left12' then
      winPosition = 'left13'
      hs.window.focusedWindow():moveToUnit({0, 0, 1/3, 1})
   else
      winPosition = 'left23'
      hs.window.focusedWindow():moveToUnit({0, 0, 2/3, 1})
   end
end

function winResizeRight()
   if winPosition == 'right23' then
      winPosition = 'right12'
      hs.window.focusedWindow():moveToUnit({1/2, 0, 1/2, 1})
   elseif winPosition == 'right12' then
      winPosition = 'right13'
      hs.window.focusedWindow():moveToUnit({2/3, 0, 1/3, 1})
   else
      winPosition = 'right23'
      hs.window.focusedWindow():moveToUnit({1/3, 0, 2/3, 1})
   end
end

function winResizeFull()
   winPosition = 'full'
   hs.window.focusedWindow():moveToUnit({0, 0, 1, 1})
end


-- Anycomplete
-- -----------------------------------------------

anycomplete = hs.loadSpoon("Anycomplete")
anycomplete.engine = "duckduckgo"
anycomplete.bindHotkeys()


-- Bindings
-- ----------------------------------------------

local applicationHotkeys = {
   m = 'Mail',
   n = 'Notes',
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
   o = pvt.openDropbox,
   f = oht.newFinderWindow,
}

local hyperHotkeys = {
   t = snipISODate,
   d = toggleDarkMode,
   j = wm_left,
   k = wm_center,
   l = wm_right,
   v = pastePlainText,
}

for key, fn  in pairs(alphaHotkeys) do hs.hotkey.bind(alpha, key, fn) end
for key, fn  in pairs(hyperHotkeys) do hs.hotkey.bind(hyper, key, fn) end
for key, app in pairs(applicationHotkeys) do
   hs.hotkey.bind(alpha, key, function() hs.application.launchOrFocus(app) end)
end

-- Similar to binding to hide/show the Dock.
hs.hotkey.bind({'alt', 'cmd'}, 'm', toggleMenubar)

hs.hotkey.bind(power, 'k', chooseMenuItem)

hs.hotkey.bind(hyper, 'left', winResizeLeft)
hs.hotkey.bind(hyper, 'right', winResizeRight)
hs.hotkey.bind(hyper, 'f', winResizeFull)


-- Custom Modal Keymap
-- ---------------------------------------------

-- Create the model keymap to bind inside of
oht.myKeys = hs.hotkey.modal.new()

-- Create the menubar item
function oht.myKeys:entered()
   myKeysMenuItem = hs.menubar.new():setTitle("Oliver's Keymap!")
   myKeysMenuItem:setTooltip("Press Escape to deactivate.")
end

-- Remove the menu item
function oht.myKeys:exited()
   myKeysMenuItem:delete()
end

-- And you need a way to activate the keymap
hs.hotkey.bind(power, 'space', function() oht.myKeys:enter() end)

-- You need a way to exit the keymap
-- And you should prevent recursion
oht.myKeys:bind('', 'escape', function() oht.myKeys:exit() end)
oht.myKeys:bind(power, 'space', function() oht.myKeys:exit() end)

-- Now bind keys into the map
oht.myKeys:bind('', 'return', function() typeHolidayFollowup() oht.myKeys:exit() end)
oht.myKeys:bind('', 'n',      function() newMailMessage()      oht.myKeys:exit() end)
oht.myKeys:bind('', 'l',      function() logbookEntry()        oht.myKeys:exit() end)


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
oht.notify("Hammerspoon", "Config reloaded, ready to rock! 🤘")


return oht

-- END HAMMERSPOON CONFIG --
