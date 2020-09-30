--[[
This uses the command-line utility 'blueutil'
to switch on/off bluetooth when your computer wakes/sleeps.

brew install blueutil

I got it from here: https://gist.github.com/ysimonson/fea48ee8a68ed2cbac12473e87134f58
]]

require "string"

function checkBluetoothResult(rc, stderr, stderr)
	if rc ~= 0 then
		print(string.format("Unexpected result executing `blueutil`: rc=%d stderr=%s stdout=%s", rc, stderr, stdout))
	end
end

function bluetooth(power)
	print("Setting bluetooth to " .. power)
	local t = hs.task.new("/usr/local/bin/blueutil", checkBluetoothResult, {"--power", power})
	t:start()
end

function f(event)
	if event == hs.caffeinate.watcher.systemWillSleep then
		bluetooth("off")
	elseif event == hs.caffeinate.watcher.screensDidWake then
		bluetooth("on")
	end
end

watcher = hs.caffeinate.watcher.new(f)
watcher:start()