-- Setup
-- -----------------------------------------------

-- HYPER is used for window management
hyper = {"ctrl", "alt", "cmd"}
-- HALF_HYPER is used for shortcuts
half_hyper = {"ctrl", "cmd"}

-- Set grid size.
hs.grid.GRIDWIDTH  = 12
hs.grid.GRIDHEIGHT = 12
hs.grid.MARGINX    = 0
hs.grid.MARGINY    = 0
-- Set window animation off. It's much smoother.
hs.window.animationDuration = 0


-- Plugins (Spoons)
-- -----------------------------------------------

-- Anycomplete Plugin
local anycomplete = require "anycomplete/anycomplete"
anycomplete.registerDefaultBindings()


-- Functions
-- -----------------------------------------------

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
    thiswindow:setFrame(loc, 0.1)
end


-- Application switching - USE 'HALF_HYPER'
-- -----------------------------------------------

hs.hotkey.bind(half_hyper, "s", function() hs.application.launchOrFocus("Safari") end)
hs.hotkey.bind(half_hyper, "c", function() hs.application.launchOrFocus("Google Chrome") end)
hs.hotkey.bind(half_hyper, "t", function() hs.application.launchOrFocus("Terminal") end)
hs.hotkey.bind(half_hyper, "i", function() hs.application.launchOrFocus("iTunes") end)
hs.hotkey.bind(half_hyper, "f", function() hs.application.launchOrFocus("Finder") end)
hs.hotkey.bind(half_hyper, "k", function() hs.application.launchOrFocus("Slack") end)


-- Shortcuts - USE 'HALF_HYPER'
-- -----------------------------------------------

-- Sleep displays
HostName = hs.host.localizedName()
if HostName == "Oliver Mini" then
	hs.hotkey.bind({}, "f10", function() hs.execute("pmset displaysleepnow") end)
else
end

-- Reload Hammerspoon config
hs.hotkey.bind( half_hyper, 'r', function() 
	hs.reload()
	hs.notify.new({title="Hammerspoon", informativeText="Configuration reloaded"}):send()
end)

-- Window Focus - USE 'HALF_HYPER'
-- -----------------------------------------------

-- Show window hints
hs.hints.style='vimperator'
hs.hotkey.bind( half_hyper, "return", function()
	hs.hints.windowHints()
end)

-- Change focused window
hs.hotkey.bind(half_hyper, 'left',  function() hs.window.focusedWindow():focusWindowWest()  end)
hs.hotkey.bind(half_hyper, 'right', function() hs.window.focusedWindow():focusWindowEast()  end)
hs.hotkey.bind(half_hyper, 'up',    function() hs.window.focusedWindow():focusWindowNorth() end)
hs.hotkey.bind(half_hyper, 'down',  function() hs.window.focusedWindow():focusWindowSouth() end)


-- Resize/Move Window - USE 'HYPER'
-- -----------------------------------------------

-- Miro's Window Management (uses HYPER)
require('position')

-- Move windows around the screen
hs.hotkey.bind(hyper, "[", function() snap_window('left')  end)
hs.hotkey.bind(hyper, "]", function() snap_window('right')  end)
-- hs.hotkey.bind(hyper, "", function() snap_window('up')    end)
-- hs.hotkey.bind(hyper, "", function() snap_window('down') end)
hs.hotkey.bind(hyper, '=', function() hs.window.centerOnScreen(hs.window.focusedWindow()) end)

hs.hotkey.bind(hyper, 'm', hs.grid.maximizeWindow)

-- -- Snap windows to the grid
-- hs.hotkey.bind(hyper, ';', function() hs.grid.snap(hs.window.focusedWindow()) end)
-- hs.hotkey.bind(hyper, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)

-- -- Move windows around the grid
-- hs.hotkey.bind(hyper, 'j', hs.grid.pushWindowDown)
-- hs.hotkey.bind(hyper, 'k', hs.grid.pushWindowUp)
-- hs.hotkey.bind(hyper, 'h', hs.grid.pushWindowLeft)
-- hs.hotkey.bind(hyper, 'l', hs.grid.pushWindowRight)

-- -- Resize windows on the grid
-- hs.hotkey.bind(hyper, ',', hs.grid.resizeWindowTaller)
-- hs.hotkey.bind(hyper, '.', hs.grid.resizeWindowShorter)
-- hs.hotkey.bind(hyper, '],', hs.grid.resizeWindowWider)
-- hs.hotkey.bind(hyper, '[', hs.grid.resizeWindowThinner)
