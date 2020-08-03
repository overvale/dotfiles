--[[ Info ---------------------------------------------------------------------

KeyCodes:

f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
f16, f17, f18, f19, f20, pad, pad*, pad+, pad/, pad-, pad=,
pad0, pad1, pad2, pad3, pad4, pad5, pad6, pad7, pad8, pad9,
padclear, padenter, return, tab, space, delete, escape, help,
home, pageup, forwarddelete, end, pagedown, left, right, down, up,
shift, rightshift, cmd, rightcmd, alt, rightalt, ctrl, rightctrl,
capslock, fn

A lot the below was taken from: github.com/jasonrudolph/keyboard

]]

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

-- custom function to make keyStrokes easier to type
keyUpDown = function(modifiers, key)
  hs.eventtap.keyStroke(modifiers, key, 0)
end

-- [ Keyboard Navigation, NOT EMACS ] ----------------------------------------------------

-- This code binds a series of keys when the frontmost application is NOT emacs.
-- It does this by watching the frontmost application and enabling/disabling
-- a series of bindings.
-- https://github.com/Hammerspoon/hammerspoon/issues/2081#issuecomment-668283868

-- this is just to make it easier to type 'keyStroke'
local function keyBind(modifiers, key)
  return function() hs.eventtap.keyStroke(modifiers, key, 0) end
end

-- Create a modal hotkey object with an absurd triggering hotkey,
-- since it will never be triggered from the keyboard
hotkeys = hs.hotkey.modal.new({"cmd", "shift", "alt"}, "F19")

-- Place all your bindings here
hotkeys:bind({'ctrl'}, ',', keyBind({'alt'}, 'left'))
hotkeys:bind({'ctrl'}, '.', keyBind({'alt'}, 'down'))
hotkeys:bind({'ctrl'}, "'", keyBind({'alt'}, 'forwarddelete'))
hotkeys:bind({'ctrl'}, ';', keyBind({'alt'}, 'delete'))
hotkeys:bind({'ctrl'}, 'u', keyBind({'cmd'}, 'delete'))

-- Define a callback function to be called when application events happen
function applicationWatcherCallback(appName, eventType, appObject)
  if (appName == "Emacs") then
    if (eventType == hs.application.watcher.activated) then
      -- Emacs just got focus, disable our hotkeys
      hotkeys:exit()
    elseif (eventType == hs.application.watcher.deactivated) then
      -- Emacs just lost focus, enable our hotkeys
      hotkeys:enter()
    end
  end
end

-- Create and start the application event watcher
watcher = hs.application.watcher.new(applicationWatcherCallback)
watcher:start()

-- Activate the modal state
hotkeys:enter()

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

-- [ Run at End ] -------------------------------------------------------------

-- This runs when this file is loaded
hs.notify.new({title='Hammerspoon', informativeText='Ready to rock 🤘'}):send()
