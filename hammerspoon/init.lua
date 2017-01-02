-- Define hyper keys
hyper = {"ctrl", "alt", "cmd"}
half_hyper = {"ctrl", "cmd"}

-- Reload Hammerspoon config
hs.hotkey.bind( hyper, 'r', function()
	hs.reload()
	hs.notify.new({title="Hammerspoon", informativeText="Configuration reloaded"}):send()
end)

-- When pressed alone, control acts like escape. Useful for Vim.
-- I've disabled this because it's so laggy
-- require('control_escape')

-- Oliver's Window Management
-- These bindings use half_hyper
-- And act on a grid
require('windows')

-- Miro's Window Management
-- These bindings use hyper
require('position')

-- Anycomplete
local anycomplete = require "anycomplete/anycomplete"
anycomplete.registerDefaultBindings()

-- hs.hotkey.bind(half_hyper, 't', function() hs.alert.show(os.date("%A %b %d, %Y - %I:%M%p"), 4) end)

-- Application switching
hs.hotkey.bind(half_hyper, "s", function() hs.application.launchOrFocus("Safari") end)
hs.hotkey.bind(half_hyper, "t", function() hs.application.launchOrFocus("Terminal") end)
hs.hotkey.bind(half_hyper, "i", function() hs.application.launchOrFocus("iTunes") end)
hs.hotkey.bind(half_hyper, "b", function() hs.application.launchOrFocus("Finder") end)

-- ---------------------- --

-- For the app "Terminal" enable/disable the function when app is focused/unfocused
-- hs.window.filter.new('Terminal')
--     :subscribe(hs.window.filter.windowFocused,function() tab2escape:enable() end)
--     :subscribe(hs.window.filter.windowUnfocused,function() tab2escape:disable() end)
