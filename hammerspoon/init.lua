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

function stringCMD(x)
   -- Executes the argument (string) as a function.
   _G[x]()
end

function simpleNotify(title, text)
  hs.notify.new({ title = title, informativeText = text}):send()
end

function keyUpDown(modifiers, key)
   hs.eventtap.keyStroke(modifiers, key, 0)
end

require("private")


-- Functions
-- -----------------------------------------------

function reloadHammerspoon() hs.reload() end

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
   hs.osascript.applescript('tell application \"BBEdit\" to (open scratchpad document) activate')
end

function backupOpenLogs () os.execute("open ~/home/src/rsync-backup/logs") end
function rsyncBackup() os.execute( "~/home/src/rsync-backup/laptop-backup.sh" ) end

function emacsDebugInit() os.execute( "~/Applications/Emacs.app/Contents/MacOS/Emacs --debug-init" ) end
function emacsQ() os.execute( "~/Applications/Emacs.app/Contents/MacOS/Emacs -q" ) end

function copyMailURL() os.execute( "~/home/dot/bin/getMailURL | pbcopy" ) end
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

function newFinderWindow()
   finder = hs.appfinder.appFromName("Finder")
   hs.osascript.applescript('tell application "Finder" to make new Finder window')
   finder:activate()
end

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


-- ----------------------------------------------



-- Readline Keymap
-- ----------------------------------------------
-- MacOS supports lots of Emacs-style shortcuts out of the box, but it is
-- missing M-f, M-b, M-d -- and I think it should also support the readline
-- shortcuts  C-u, C-w.
-- https://readline.kablamo.org/emacs.html
-- However, rather than just binding them globally, I want to switch them off
-- when Emacs and the Terminal are the foreground app.

ReadlineKeymap = hs.hotkey.modal.new()

ReadlineKeymap:bind({'ctrl'}, 'w', function() keyUpDown({'alt'}, 'delete') end)
ReadlineKeymap:bind({'ctrl'}, 'u', function() keyUpDown({'cmd'}, 'delete') end)
ReadlineKeymap:bind({'alt'},  'd', function() keyUpDown({'alt'}, 'forwarddelete') end)
ReadlineKeymap:bind({'alt'},  'b', function() keyUpDown({'alt'}, 'left') end)
ReadlineKeymap:bind({'alt'},  'f', function() keyUpDown({'alt'}, 'right') end)


-- Activate App-Specific Keymaps
-- ------------------------------------------------

function setUserKeymaps()
   -- This function is called every time any app is activated.
   -- At the moment it just activates the readline keymap but
   -- any number of keymaps for any number of apps could be activated here.
   if appTitle() == "Emacs" or appTitle() == "Terminal" then
      -- print( appTitle() .. ': ' .. 'Readline keymap exited.')
      ReadlineKeymap:exit()
   else
      -- print(appTitle() .. ': ' .. 'Readline keymap entered.')
      ReadlineKeymap:enter()
   end
end

function appWatcherFunction(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      -- Activate all my user keymaps
      setUserKeymaps()
      -- Teams has an annoying bug that doesn't foreground the main window
      -- when you switch to the app, this fixes it:
      if (appName == "Microsoft Teams") then
         appObject:selectMenuItem({"Window", "Bring All to Front"})
      end
   end
end

setUserKeymaps()
print('User Keymaps activated.')

hs.application.watcher.new(appWatcherFunction):start()
print('Application Watcher started.')


-- M-x Anything
-- ----------------------------------------------

-- Initialize the table of choices used by the chooser
mxChoices = {}

-- This table will be reformatted and inserted into mxChoices.
-- This is done so that I can write it in this simpler format, rather than the
-- cumbersome one required by the chooser.
mxChoiceTable = {
   { "path", "Desktop",                                "~/Desktop" },
   { "path", "Downloads",                              "~/Downloads" },
   { "path", "Home",                                   "~/home" },
   { "path", "iCloud Documents",                       "~/Library/Mobile Documents/com~apple~CloudDocs/Oliver" },
   { "app",  "Activity Monitor", "Activity Monitor" },
   { "app",  "Arc", "Arc" },
   { "app",  "BBEdit", "BBEdit" },
   { "app",  "Calendar", "Calendar" },
   { "app",  "Clock", "Clock" },
   { "app",  "Console", "Console" },
   { "app",  "Contacts", "Contacts" },
   { "app",  "Dictionary", "Dictionary" },
   { "app",  "Emacs", "Emacs" },
   { "app",  "FaceTime", "FaceTime" },
   { "app",  "Find My", "Find My" },
   { "app",  "Firefox", "Firefox" },
   { "app",  "Font Book", "Font Book" },
   { "app",  "Hammerspoon", "Hammerspoon" },
   { "app",  "Keychain Access", "Keychain Access" },
   { "app",  "Keynote", "Keynote" },
   { "app",  "Mail", "Mail" },
   { "app",  "Maps", "Maps" },
   { "app",  "Messages", "Messages" },
   { "app",  "Microsoft Excel", "Microsoft Excel" },
   { "app",  "Microsoft Outlook", "Microsoft Outlook" },
   { "app",  "Microsoft Teams", "Microsoft Teams" },
   { "app",  "Microsoft Word", "Microsoft Word" },
   { "app",  "Muse", "Muse" },
   { "app",  "Music", "Music" },
   { "app",  "Notes", "Notes" },
   { "app",  "Numbers", "Numbers" },
   { "app",  "Pages", "Pages" },
   { "app",  "Photos", "Photos" },
   { "app",  "Pixelmator", "Pixelmator" },
   { "app",  "Preview", "Preview" },
   { "app",  "QuickTime Player", "QuickTime Player" },
   { "app",  "Reminders", "Reminders" },
   { "app",  "Safari", "Safari" },
   { "app",  "Script Editor", "Script Editor" },
   { "app",  "Shortcuts", "Shortcuts" },
   { "app",  "System Information", "System Information" },
   { "app",  "System Settings", "System Settings" },
   { "app",  "TV", "TV" },
   { "app",  "Tailscale", "Tailscale" },
   { "app",  "Terminal", "Terminal" },
   { "app",  "TextEdit", "TextEdit" },
   { "app",  "Tot", "Tot" },
   { "app",  "Transmit", "Transmit" },
   { "app",  "Voice Memos", "Voice Memos" },
   { "app",  "Weather", "Weather" },
   { "app",  "iA Writer", "iA Writer" },
   { "app",  "zoom.us", "zoom.us" },
   { "func", "New Finder Window",                      "newFinderWindow" },
   { "func", "Edit Hammerspoon Config",                "editHammerspoonInit" },
   { "func", "Open Rsync Backup Logs",                 "backupOpenLogs" },
   { "func", "Open BBEdit Scratch",                    "bbeditScratch" },
   { "func", "Choose Menu Item",                       "chooseMenuItem" },
   { "func", "Copy Mail Message URL",                  "copyMailURL" },
   { "func", "Kill Work Apps",                         "killWorkApps" },
   { "func", "Show Meeting Times",                     "meetingTimes" },
   { "func", "New Mail Message",                       "newMailMessage" },
   { "func", "Open Dropbox Bid folder",                "openDropbox" },
   { "func", "Open Work Apps",                         "openWorkApps" },
   { "func", "Paste as Plain Text",                    "pastePlainText" },
   { "func", "Reload Hammerspoon",                     "reloadConfig" },
   { "func", "Start Rsync Backup",                     "rsyncBackup" },
   { "func", "Search GitHub",                          "searchGitHub" },
   { "func", "Search IMDB",                            "searchIMDB" },
   { "func", "Search Wikipedia",                       "searchWikipedia" },
   { "func", "Search Youtube",                         "searchYouTube" },
   { "func", "Snippet: ISO Date",                      "snipISODate" },
   { "func", "Snippet: Org Mode Date",                 "snipOrgDate" },
   { "func", "Snippet ¯\\_(ツ)_/¯",                    "snipShrug" },
   { "func", "Snippet \"waving hands around\"",        "snipWave" },
   { "func", "Toggle Dark Mode",                       "toggleDarkMode" },
   { "func", "Toggle MenuBar",                         "toggleMenubar" },
   { "func", "Type Current Safari URL",                "typeCurrentSafariURL" },
   { "func", "Type Execs + MDs + EPs email addresses", "typeExecMDsEPs" },
   { "func", "Open Excel Scratch Doc",                 "scratchExcel" },
   { "func", "Type Work Email",                        "typeWorkEmail" },
   { "func", "Reading Tabs",                           "readingTabs" }
}

-- Now iterate over mxChoiceTable and insert all the table items into the
-- table used by the chooser (mxChoices).
for i, mapping in ipairs(mxChoiceTable) do
   local type = mapping[1]
   local text = mapping[2]
   local arg  = mapping[3]
   table.insert(mxChoices, {["type"] = type, ["text"] = text, ["arg"] = arg})
end

-- Create the actual chooser and define what happens when you select an item
-- from it.
mxChooser = hs.chooser.new(function(choice)
      if not choice then
         return
      else
         if choice["type"] == "app" then
            os.execute("open -a" .. "'" .. choice["arg"] .. "'")
         elseif choice["type"] == "path" then
            os.execute("open " .. choice["arg"])
         elseif choice["type"] == "func" then
            _ENV[choice["arg"]]()
         end
      end
end)

mxChooser:choices(mxChoices)
mxChooser:placeholderText("M-x Hammerspoon")
mxChooser:bgDark(true)

hs.hotkey.bind(alpha, "x", function() mxChooser:show() end)


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

keyBindings = {
   { alpha, 'm', 'Mail' },
   { alpha, 'n', 'Notes' },
   { alpha, 'c', 'Calendar' },
   { alpha, 'b', 'BBEdit' },
   { alpha, 'e', 'Emacs' },
   { alpha, 's', 'Safari' },
   { alpha, 'a', 'Music' },
   { alpha, 't', 'Terminal' },
   { alpha, 'r', 'Reminders' },
   { alpha, 'h', reloadHammerspoon },
   { alpha, 'o', openDropbox },
   { alpha, 'f', newFinderWindow },
   { hyper, 't', snipISODate },
   { hyper, 'd', toggleDarkMode },
   { hyper, 'left', winResizeLeft },
   { hyper, 'right', winResizeRight },
   { hyper, 'f', winResizeFull },
   { hyper, 'j', wm_left },
   { hyper, 'k', wm_center },
   { hyper, 'l', wm_right },
   { hyper, 'v', pastePlainText },
   { power, 'k', chooseMenuItem},
   { {'alt', 'cmd'}, 'm', toggleMenubar },
}

for i, mapping in ipairs(keyBindings) do
   local mod = mapping[1]
   local key = mapping[2]
   local app = mapping[3]
   hs.hotkey.bind(mod, key, function()
     if (type(app) == 'string') then
            hs.application.launchOrFocus(app)
     else
        app()
     end
   end)
end


-- Custom Modal Keymap
-- ---------------------------------------------

-- Create the model keymap to bind inside of
transientKeys = hs.hotkey.modal.new()

-- Create the menubar item
function transientKeys:entered()
   myKeysMenuItem = hs.menubar.new():setTitle("Oliver's Keymap!")
   myKeysMenuItem:setTooltip("Press Escape to deactivate.")
end

-- Remove the menu item
function transientKeys:exited()
   myKeysMenuItem:delete()
end

transientKeysBindings = {
   { {}, 'n', newMailMessage },
   { {}, 'l', logbookEntry},
   { {}, 'return', typeHolidayFollowup },
}

do -- Set the binding and provide an escape, while preventing recursion.
   local mod = power
   local key = 'space'
   hs.hotkey.bind(mod, key, function() transientKeys:enter() end)
   transientKeys:bind(mod, key, function() transientKeys:exit() end)
   transientKeys:bind('', 'escape', function() transientKeys:exit() end)
end

for i, mapping in ipairs(transientKeysBindings) do
   local mod = mapping[1]
   local key = mapping[2]
   local fn  = mapping[3]
   transientKeys:bind(mod, key, function() fn() transientKeys:exit() end)
end


-- Notify on Successful Load
-- -------------------------------------------

simpleNotify('Hammerspoon', 'Hammerspoon loaded successfully!')


-- END HAMMERSPOON CONFIG --
