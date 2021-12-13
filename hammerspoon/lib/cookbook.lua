--[[ NOTE:

This is a scratchpad with which I am learning hammerspoon.
Not all the code here actually works.

]]


-- App-Specific Bindings
-- -----------------------------------------------

-- Remote Desktop
local msrdDisable = hs.hotkey.new({"cmd"}, "w", function()
      -- nil
end)

-- Excel
local excelDown = hs.hotkey.new({"ctrl"}, "n", function()
      hs.eventtap.event.newKeyEvent({}, "down", true):post()
      hs.eventtap.event.newKeyEvent({}, "down", false):post()
end)
local excelUp = hs.hotkey.new({"ctrl"}, "p", function()
      hs.eventtap.event.newKeyEvent({}, "up", true):post()
      hs.eventtap.event.newKeyEvent({}, "up", false):post()
end)

msrdDisable:disable()
excelDown:disable()
excelUp:disable()

if appName == "Microsoft Remote Desktop" then
   if eventType == hs.application.watcher.activated then
      msrdDisable:enable()
   elseif eventType == hs.application.watcher.deactivated or eventType == hs.application.watcher.terminated then
      msrdDisable:disable()
   end
elseif appName == "Microsoft Excel" then
   if eventType == hs.application.watcher.activated then
      excelUp:enable()
      excelDown:enable()
   elseif eventType == hs.application.watcher.deactivated or eventType == hs.application.watcher.terminated then
      excelUp:disable()
      excelDown:disable()
   end
end



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


-- Automatic Reload Config
-- -----------------------------------------------

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


-- Click on any menu item
-- --------------------------------------------

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "E", function()
  -- emulates a click
  hs.application.get("Hammerspoon"):selectMenuItem("Console...")
  hs.application.launchOrFocus("Hammerspoon")
end)


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


