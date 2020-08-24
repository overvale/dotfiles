-- Stolen/Adapted from: https://github.com/raulchen/dotfiles/blob/master/hammerspoon/smart_modifier_keys.lua
-- This version adds the `cmd` and `alt` modifiers.
-- FINALLY! A simple way to modify modifiers-when-pressed-alone.
-- I've been searching for a good, extensible,mm version of this for a long time.
-- CAUTION: enabling "Secure Keyboard Entry" in Terminal.app breaks this script.

local module = {}

-- Whether ctrl and shift is being pressed alone.
module.ctrlPressed = false
module.shiftPressed = false
module.cmdPressed = false
module.altPressed = false

module.prevModifiers = {}

module.log = hs.logger.new('smart_modifier_keys','debug')

module.modifierKeyListener = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, function(e)
    local events_to_post = nil

    local modifiers = e:getFlags()
    local count = 0
    for _, __ in pairs(modifiers) do
        count = count + 1
    end

    -- Check `ctrl` key.
    if modifiers['ctrl'] and not module.prevModifiers['ctrl'] and count == 1 then
        module.ctrlPressed = true
    else
        if count == 0 and module.ctrlPressed then
            -- Ctrl was tapped alone, send key.
            events_to_post = {
	       hs.eventtap.event.newKeyEvent({"cmd"}, "space", true),
	       hs.eventtap.event.newKeyEvent({"cmd"}, "space", false),
            }
        end
        module.ctrlPressed = false
    end

    -- Check `shift` key.
    if modifiers['shift'] and not module.prevModifiers['shift'] and count == 1 then
        module.shiftPressed = true
    else
        if count == 0 and module.shiftPressed then
            -- Shift was tapped alone, send key.
            events_to_post = {
                hs.eventtap.event.newKeyEvent(nil, "delete", true),
                hs.eventtap.event.newKeyEvent(nil, "delete", false),
            }
        end
        module.shiftPressed = false
    end

    -- Check `command` key.
    if modifiers['cmd'] and not module.prevModifiers['cmd'] and count == 1 then
        module.cmdPressed = true
    else
        if count == 0 and module.cmdPressed then
            -- Command was tapped alone, send key.
            events_to_post = {
                hs.eventtap.event.newKeyEvent({"alt"}, "delete", true),
                hs.eventtap.event.newKeyEvent({"alt"}, "delete", false),
            }
        end
        module.cmdPressed = false
    end

    -- Check `alt` key.
    if modifiers['alt'] and not module.prevModifiers['alt'] and count == 1 then
        module.altPressed = true
    else
        if count == 0 and module.altPressed then
            -- Alt was tapped alone, send key.
            events_to_post = {
                hs.eventtap.event.newKeyEvent({"alt"}, "forwarddelete", true),
                hs.eventtap.event.newKeyEvent({"alt"}, "forwarddelete", false),
            }
        end
        module.altPressed = false
    end

    module.prevModifiers = modifiers
    return false, events_to_post
end):start()


module.normalKeyListener = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(e)
    -- If a non-modifier key is pressed, reset these two flags.
    module.ctrlPressed = false
    module.shiftPressed = false
    module.cmdPressed = false
    module.altPressed = false
end):start()

return module
