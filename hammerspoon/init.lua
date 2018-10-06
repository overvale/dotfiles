-- Setup
-- -----------------------------------------------

-- HYPER is used for window management
hyper = {"ctrl", "alt", "cmd"}
-- HALF_HYPER is used for shortcuts
half_hyper = {"ctrl", "cmd"}


-- Snippets
-- -----------------------------------------------

-- When you press C-` type the path to the home folder.
-- Useful for when ~ doesn't work (looking at you ACME).
hs.hotkey.bind( "ctrl", "`", function()
	hs.eventtap.keyStrokes("/Users/olivertaylor/")
end)



-- Plugins
-- -----------------------------------------------

-- Anycomplete Plugin
local anycomplete = require "anycomplete"
anycomplete.registerDefaultBindings()

require('position')

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

-- Reload Hammerspoon config
hs.hotkey.bind( half_hyper, 'r', function() 
	hs.reload()
	hs.notify.new({title="Hammerspoon", informativeText="Configuration reloaded"}):send()
end)

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

-- Lauch Apps
-- Don't use "f" (full-screen) or "r"
local appList = {
	s = 'Safari',
	c = 'Google Chrome',
	t = 'Terminal',
	i = 'iTunes',
	k = 'Slack',
	m = 'Mail',
	e = 'Emacs',
	b = 'BBEdit',
}

for key, app in pairs(appList) do
	hs.hotkey.bind(half_hyper, key, function() hs.application.launchOrFocus(app) end)
end

-- Open Folder
hs.hotkey.bind(hyper, "b", function()
	hs.execute("open /Users/olivertaylor/Dropbox_Ingenuity/Bidding/")
end)

hs.hotkey.bind(hyper, "r", function()
	hs.execute("open /Volumes/ramburglar_work/")
end)


-- Resize/Move Window - USE 'HYPER'
-- -----------------------------------------------

-- Hyper + arrows taken by 'position.lua'
-- Hyper 'g' taken by anycomplete plugin

-- Move windows around the screen
hs.hotkey.bind(hyper, "[", function() snap_window('left') end)
hs.hotkey.bind(hyper, "]", function() snap_window('right') end)
hs.hotkey.bind(hyper, '=', function() hs.window.centerOnScreen(hs.window.focusedWindow()) end)


-- Bindings
-- -----------------------------------------------
