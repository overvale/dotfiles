--[[ NOTE:

This is a scratchpad with which I am learning hammerspoon.
Not all the code here actually works.

]]

-- [ Learning! ]-------------------------------------------------------------

hs.hotkey.bind({'cmd', 'ctrl'}, '1', function() hs.alert.show("alert") end)

function oliverDown()
   hs.alert.show("down")
end
function oliverUp()
   hs.alert.show("up")
end

hs.hotkey.bind({'cmd', 'ctrl'}, '3', oliverDown, oliverUp)

function spaceDown()
   hs.alert.show("down")
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, true):post()
   hs.eventtap.event.newKeyEvent('e', true):post()
   hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, false):post()
end
function spaceUp()
   hs.eventtap.keyStroke({}, 'return', 0)
end

hs.hotkey.bind({'cmd', 'ctrl'}, '4', spaceDown, spaceUp)

-- [ Mode Test ] ---------------------------------------------------

function modeTestStart()
   hs.alert.show("Mode Entered")
   testModal:enter()
end
function modeTestEnd()
   hs.alert.show("Mode Exited")
   testModal:exit()
end

testModal = hs.hotkey.modal.new({'cmd', 'ctrl', 'alt'}, 'F18')
testModal:bind({''}, 'e', keyStrike({'ctrl'}, 'e'))
hs.hotkey.bind({'cmd', 'ctrl'}, '2', modeTestStart, modeTestEnd)

-- Toggle Application
-- -------------------------------------------

mash = {'cmd', 'alt', 'ctrl'}

local function toggleApplication(name)
  local app = hs.application.find(name)
  if not app or app:isHidden() then
    hs.application.launchOrFocus(name)
  elseif hs.application.frontmostApplication() ~= app then
    app:activate()
  else
    app:hide()
  end
end

hs.hotkey.bind(mash, "c", function() toggleApplication("Google Chrome") end)
hs.hotkey.bind(mash, "d", function() toggleApplication("Dash") end)
hs.hotkey.bind(mash, "f", function() toggleApplication("Finder") end)
hs.hotkey.bind(mash, "g", function() toggleApplication("SourceTree") end)
hs.hotkey.bind(mash, "m", function() toggleApplication("Mail") end)
hs.hotkey.bind(mash, "p", function() toggleApplication("System Preferences") end)
hs.hotkey.bind(mash, "s", function() toggleApplication("Spotify") end)
hs.hotkey.bind(mash, "t", function() toggleApplication("Terminal") end)

-- VIM Mode
-- -----------------------------------------------

local normal = hs.hotkey.modal.new()

enterNormal = hs.hotkey.bind({"ctrl"}, "[", function()
      normal:enter()
      hs.alert.show('Normal mode')
end)

function left() hs.eventtap.keyStroke({}, "Left") end
normal:bind({}, 'h', left, nil, left)

function right() hs.eventtap.keyStroke({}, "Right") end
normal:bind({}, 'l', right, nil, right)

function up() hs.eventtap.keyStroke({}, "Up") end
normal:bind({}, 'k', up, nil, up)

function down() hs.eventtap.keyStroke({}, "Down") end
normal:bind({}, 'j', down, nil, down)

normal:bind({}, 'i', function()
    normal:exit()
    hs.alert.show('Insert mode')
end)

-- Arrow Delete
-- -----------------------------------------------

-- This makes it so fn+mods+left/right acts as a 'delete' modifer.
-- This way you can zip around with the arrow keys and add the 'fn' key to delete
-- while your hands are still on the arrows.

hs.hotkey.bind({},      'home', function() hs.eventtap.keyStroke({},      'delete') end)
hs.hotkey.bind({'alt'}, 'home', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
hs.hotkey.bind({'cmd'}, 'home', function() hs.eventtap.keyStroke({'cmd'}, 'delete') end)
hs.hotkey.bind({},      'end',  function() hs.eventtap.keyStroke({},      'forwarddelete') end)
hs.hotkey.bind({'alt'}, 'end',  function() hs.eventtap.keyStroke({'alt'}, 'forwarddelete') end)
hs.hotkey.bind({'cmd'}, 'end',  function() hs.eventtap.keyStroke({'ctrl'}, 'k') end)

-- Window Switcher
-- -----------------------------------------------
-- https://github.com/raulchen/dotfiles/tree/master/hammerspoon

local switcher = hs.window.switcher.new(nil, {
    fontName = ".AppleSystemUIFont",
    textSize = 16,
    textColor = { white = 0, alpha = 1 },
    highlightColor = { white = 0.5, alpha = 0.3 },
    backgroundColor = { white = 0.95, alpha = 0.9 },
    titleBackgroundColor = { white = 0.95, alpha = 0 },
    showThumbnails = true,
    showSelectedThumbnail = false,
})

local function nextWindow()
    switcher:next()
end

local function previousWindow()
    switcher:previous()
end

hs.hotkey.bind('alt', 'tab', nextWindow, nil, nextWindow)
hs.hotkey.bind('alt-shift', 'tab', previousWindow, nil, previousWindow)


-- Bring all Finder windows to front when finder activated
-- -----------------------------------------------
function applicationWatcher(appName, eventType, appObject)
    if (eventType == hs.application.watcher.activated) then
        if (appName == "Finder") then
            -- Bring all Finder windows forward when one gets activated
            appObject:selectMenuItem({"Window", "Bring All to Front"})
        end
    end
end
appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()


-- Remap key per application
-- -----------------------------------------------
-- Remap a key one way in one application, and another in all others
-- LIMITATION: You can't remap a binding to the same binding,
-- hammerspoon will catch the keypress again and try to remap it again.
-- Hammerspoon also ALWAYS catches the bound key, so there's no way to
-- fall back to the same key that you've bound.
-- So leaving the 'else' statement below blank would result in
-- 'ctrl n' being a dead key outside Emacs.

hs.hotkey.bind({"ctrl"}, "j", function()
      local app = hs.application.frontmostApplication()
      if app:name() == "Emacs" then
	 hs.eventtap.keyStroke({"ctrl"}, "n")
      else
	 hs.eventtap.event.newKeyEvent({}, "down", true):post()
	 hs.eventtap.event.newKeyEvent({}, "down", false):post()
      end
end)

hs.hotkey.bind({"shift", "cmd"}, "/", function()
      local app = hs.application.frontmostApplication()
      if app:name() == "Emacs" then
	 hs.eventtap.event.newKeyEvent({"ctrl"}, "h", true):post()
	 hs.eventtap.event.newKeyEvent({"ctrl"}, "h", false):post()
      else
	 hs.application.frontmostApplication():selectMenuItem({"Help"})
      end
end)


-- Name of Wifi in menu bar
-- ----------------------------------------------
wifiWatcher = nil
function ssidChanged()
   local wifiName = hs.wifi.currentNetwork()
   if wifiName then
      wifiMenu:setTitle(wifiName)
   else 
      wifiMenu:setTitle("Wifi OFF")
   end
end
wifiMenu = hs.menubar.newWithPriority(2147483645)
ssidChanged()
wifiWatcher = hs.wifi.watcher.new(ssidChanged):start()


-- RAMEN TIMER
-- ----------------------------------------------

--Schedule a notification in 3 minutes.
function startRamenTimer()
  hs.timer.doAfter(3 * 60, function ()
    hs.notify.new({
        title="Ramen time!",
        informativeText="Your ramen is ready!"
    }):send()
  end)
  hs.alert(" Ramen timer started! ")
end

--Bind timer to `hammerspoon://ramentime`:
hs.urlevent.bind("ramentime", startRamenTimer)


-- Toggle dark mode from menu bar
-- ----------------------------------------------

local function systemSetDm(state)
  return hs.osascript.javascript(
    string.format(
      "Application('System Events').appearancePreferences.darkMode.set(%s)",
      state
    )
  )
end

local function dmIsOn()
  local _, darkModeState = hs.osascript.javascript(
    'Application("System Events").appearancePreferences.darkMode()'
  )
  return darkModeState
end

function setDm(state)
	systemSetDm(state)
    if state then
        darkmode:setTitle("☾")
    else
        darkmode:setTitle("☀")
    end
end

function darkmodeClicked()
    setDm(not dmIsOn())
end

darkmode = hs.menubar.new()
if darkmode then
    darkmode:setClickCallback(darkmodeClicked)
    setDm(dmIsOn())
end


-- Run functions from the menubar
-- -----------------------------------------------
function test_notify()
   hs.notify.new({title='Hammerspoon', informativeText='Test worked!'}):send()
end
local menuTable = {
    { title = "Test", fn = test_notify },
    -- { title = "-" },
}
local menubar = hs.menubar.new()
if menubar then
    menubar:setTitle("⌘")
    menubar:setMenu(menuTable)
end

-- Custom clock in menubar
-- -----------------------------------------------
local clockMenu = hs.menubar.new()
function displayClock(clockMenu)
  clockTime = os.date("%Y-%m-%d, %H:%M")
  clockMenu:setTitle(clockTime)
end
-- Make the menu show up on load
displayClock(clockMenu)
-- And refresh it every so often (I don't care about seconds)
hs.timer.doEvery(60, function() displayClock(clockMenu) end)


-- BACKUP MENU
-- -----------------------------------------------

-- This places icons in your menubar that indicate the status of your backups.
-- The icons are updated by your backup script.
-- If the backup is successful the backup script opens 'hammerspoon://backup_success'
-- which when displays an icon in your menubar.

-- Create the menubar item
local backupMenu = hs.menubar.new()

-- Define functions for displaying icons, and behavior
function backup_openLogs()
   os.execute( "open ~/Library/" )
end
function backup_running()
   backupMenu:setTitle("🚂")
   hs.timer.doAfter(1, backup_success)
end
function backup_success()
   backupMenu:setTitle("🏆")
   hs.timer.doAfter(1, backup_next)
end
function backup_next()
   local menuTable = {
      { title = "Backup Now", fn = backup_running },
      { title = "Open Logs", fn = backup_openLogs },
   }
   backupMenu:setTitle("😎")
   backupMenu:setMenu(menuTable)
end
function backup_fail()
   local menuTable = {
      { title = "Retry Backup", fn = backup_running },
      { title = "Open Logs", fn = backup_openLogs },
   }
   backupMenu:setTitle("⚠️")
   backupMenu:setMenu(menuTable)
end

-- Run a function to place an item in the menubar
backup_next()

-- Register URLs and bind them to the above functions
hs.urlevent.bind("backup_running", backup_running)
hs.urlevent.bind("backup_success", backup_success)
hs.urlevent.bind("backup_next", backup_next)
hs.urlevent.bind("backup_fail", backup_fail)

