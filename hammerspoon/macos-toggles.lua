---- Menu Bar ----

function toggleMenubar()
  hs.applescript([[
	tell application "System Events"
	  tell dock preferences to set autohide menu bar to not autohide menu bar
	end tell]])
end

---- Dark Mode ----

function darkModeStatus()
   -- return the status of Dark Mode
   local _, darkModeState = hs.osascript.javascript(
	  'Application("System Events").appearancePreferences.darkMode()'
   )
   return darkModeState
end

function setDarkMode(state)
   -- Function for setting Dark Mode on/off.
   -- Argument should be either 'true' or 'false'.
   return hs.osascript.javascript(
	  string.format(
		 "Application('System Events').appearancePreferences.darkMode.set(%s)", state
   ))
end

function toggleDarkMode()
   -- Toggle Dark Mode status
   -- Argument should be either 'true' or 'false'.
   if darkModeStatus() then
	  setDarkMode(false)
   else
	  setDarkMode(true)
   end
end

---- Stage Manager ----

function utilTrim(str)
  return str:match("^()%s*$") and "" or str:match("^%s*(.*%S)")
end

function stageManStatus()
   local stageManState = hs.execute("/usr/bin/defaults read com.apple.WindowManager GloballyEnabled")
   return utilTrim(stageManState) == "1"
end

function setStageMan(state)
   -- Function for setting Stage Manger on/off.
   -- Argument should be either 'true' or 'false'.
   hs.execute(string.format("/usr/bin/defaults write com.apple.WindowManager GloballyEnabled -bool %s", state) )
end

function toggleStageMan()
   -- Toggle Stage Manager status
   if stageManStatus() then
	  setStageMan(false)
   else
	  setStageMan(true)
   end
end
