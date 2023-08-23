-- Bindings
-- ---------------------------------------------
-- This binds keys to functions in the 'keyBindings' list.

keyBindings = {}

function keyBindingsSet ()
  for i, mapping in ipairs(keyBindings) do
    local mod = mapping[1]
    local key = mapping[2]
    local fn  = mapping[3]
    hs.hotkey.bind(mod, key, function() fn() end)
  end
end
