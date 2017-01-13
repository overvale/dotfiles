-- Set grid size.
hs.grid.GRIDWIDTH  = 12
hs.grid.GRIDHEIGHT = 12
hs.grid.MARGINX    = 0
hs.grid.MARGINY    = 0
-- Set window animation off. It's much smoother.
hs.window.animationDuration = 0

-- Snap windows to the grid
hs.hotkey.bind(half_hyper, ';', function() hs.grid.snap(hs.window.focusedWindow()) end)
hs.hotkey.bind(half_hyper, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)

hs.hotkey.bind(half_hyper, 'm', hs.grid.maximizeWindow)

hs.hotkey.bind(half_hyper, 'c', function() hs.window.centerOnScreen(hs.window.focusedWindow()) end)

hs.hotkey.bind(half_hyper, 'j', hs.grid.pushWindowDown)
hs.hotkey.bind(half_hyper, 'k', hs.grid.pushWindowUp)
hs.hotkey.bind(half_hyper, 'h', hs.grid.pushWindowLeft)
hs.hotkey.bind(half_hyper, 'l', hs.grid.pushWindowRight)

hs.hotkey.bind(half_hyper, ',', hs.grid.resizeWindowTaller)
hs.hotkey.bind(half_hyper, '.', hs.grid.resizeWindowShorter)
hs.hotkey.bind(half_hyper, ']', hs.grid.resizeWindowWider)
hs.hotkey.bind(half_hyper, '[', hs.grid.resizeWindowThinner)

hs.hotkey.bind(half_hyper, 'left',  function() hs.window.focusedWindow():focusWindowWest()  end)
hs.hotkey.bind(half_hyper, 'right', function() hs.window.focusedWindow():focusWindowEast()  end)
hs.hotkey.bind(half_hyper, 'up',    function() hs.window.focusedWindow():focusWindowNorth() end)
hs.hotkey.bind(half_hyper, 'down',  function() hs.window.focusedWindow():focusWindowSouth() end)

-- Show window hints
hs.hints.style='vimperator'
hs.hotkey.bind( half_hyper, "return", function()
	hs.hints.windowHints()
end)
