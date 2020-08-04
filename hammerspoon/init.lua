--[[ Info ---------------------------------------------------------------------

KeyCodes:
f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
f16, f17, f18, f19, f20, pad, pad*, pad+, pad/, pad-, pad=,
pad0, pad1, pad2, pad3, pad4, pad5, pad6, pad7, pad8, pad9,
padclear, padenter, return, tab, space, delete, escape, help,
home, pageup, forwarddelete, end, pagedown, left, right, down, up,
shift, rightshift, cmd, rightcmd, alt, rightalt, ctrl, rightctrl,
capslock, fn

http://github.com/jasonrudolph/keyboard
http://github.com/dbmrq/dotfiles/

---------------------------------------------------------------------------- ]]


-- [ Automatically reload the config when the path changes ] ------------------

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
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()


-- [ Functions ] -----------------------------------------------------------------

-- this is just to make it easier to type 'keyStroke'
local function keyStrike(modifiers, key)
  return function() hs.eventtap.keyStroke(modifiers, key, 0) end
end


-- [ Application Launcher ] ---------------------------------------------------

-- Don't use "f" (full-screen)
local appList = {
	s = 'Safari',
	t = 'Terminal',
	a = 'Music',
	m = 'Mail',
	e = 'Emacs',
	b = 'BBEdit',
}

for key, app in pairs(appList) do
	hs.hotkey.bind({'ctrl', 'cmd'}, key, function() hs.application.launchOrFocus(app) end)
end


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


-- [ Arrow Delete ] -------------------------------------------------------------

-- This makes it so fn+mods+left/right acts as a 'delete' modifer.
-- This way you can zip around with the arrow keys and add the 'fn' key to delete
-- while your hands are still on the arrows.

hs.hotkey.bind({},      'home', function() hs.eventtap.keyStroke({},      'delete') end)
hs.hotkey.bind({'alt'}, 'home', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
hs.hotkey.bind({'cmd'}, 'home', function() hs.eventtap.keyStroke({'cmd'}, 'delete') end)
hs.hotkey.bind({},      'end',  function() hs.eventtap.keyStroke({},      'forwarddelete') end)
hs.hotkey.bind({'alt'}, 'end',  function() hs.eventtap.keyStroke({'alt'}, 'forwarddelete') end)
hs.hotkey.bind({'cmd'}, 'end',  function() hs.eventtap.keyStroke({'ctrl'}, 'k') end)


-- [ Word Move/Delete ] -------------------------------------------------------------

-- This, remember, is at the system level, so Terminal/Emacs will need to accept
-- things like alt+delete and cmd+delete.

hs.hotkey.bind({'ctrl'}, 'w', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
hs.hotkey.bind({'ctrl'}, 'u', function() hs.eventtap.keyStroke({'cmd'}, 'delete') end)
hs.hotkey.bind({'ctrl'}, ';', function() hs.eventtap.keyStroke({'alt'}, 'left') end)
hs.hotkey.bind({'ctrl'}, "'", function() hs.eventtap.keyStroke({'alt'}, 'right') end)
hs.hotkey.bind({'ctrl'}, ',', function() hs.eventtap.keyStroke({'alt'}, 'delete') end)
hs.hotkey.bind({'ctrl'}, '.', function() hs.eventtap.keyStroke({'alt'}, 'forwarddelete') end)


-- [ Run at End ] -------------------------------------------------------------

-- This runs when this file is loaded to confirm it is working
hs.notify.new({title='Hammerspoon', informativeText='Ready to rock 🤘'}):send()
