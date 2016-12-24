-- Define hyper keys
hyper = {"ctrl", "alt", "cmd"}
half_hyper = {"ctrl", "cmd"}

-- Reload Hammerspoon config
hs.hotkey.bind( hyper, 'r', function()
	hs.reload()
	hs.notify.new({title="Hammerspoon", informativeText="Configuration reloaded"}):send()
end)

hs.hotkey.bind( half_hyper, "return", function()
	hs.hints.windowHints()
end)

-- When pressed alone, control acts like escape. Useful for Vim.
require('control_escape')

-- Oliver's Window Management
-- These bindings use half_hyper
require('windows')

-- Miro's Window Management
-- These bindings use hyper
require('position')