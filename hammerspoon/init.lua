--[[ Oliver Taylor's Hammerspoon Config
homepage: https://github.com/olivertaylor/dotfiles

Acceptable keycodes are here:
https://www.hammerspoon.org/docs/hs.keycodes.html#map

Inspiration:
https://spinscale.de/posts/2016-11-08-creating-a-productive-osx-environment-hammerspoon.html
https://medium.com/@robhowlett/hammerspoon-the-best-mac-software-youve-never-heard-of-40c2df6db0f8
https://github.com/jasonrudolph/keyboard
https://github.com/dbmrq/dotfiles
https://github.com/raulchen/dotfiles
https://github.com/justintanner/universal-emacs-keybindings
https://github.com/dbalatero/dotfiles/tree/master/hammerspoon
https://github.com/senorprogrammer/hammerspoon_init/

--]]

-- Setup
-- -----------------------------------------------

local hyper = {'cmd', 'alt', 'ctrl'}
local alpha = {'cmd', 'ctrl'}
local power = {'cmd', 'shift'}

hs.window.animationDuration = 0

function keyUpDown(modifiers, key)
   hs.eventtap.keyStroke(modifiers, key, 0)
end

require("private")


-- Functions
-- -----------------------------------------------

-- TODO: Add to menubad item

function pastePlainText()
   p = hs.pasteboard.readDataForUTI(nil, "public.utf8-plain-text")
   hs.pasteboard.setContents(p)
   app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Paste"})
end

function copyMailURL() os.execute( "~/home/dot/bin/getMailURL | pbcopy" ) end
function newMailMessage() os.execute("open mailto:") end

function snipWave() hs.eventtap.keyStrokes("(waving hands around)") end
function snipShrug() hs.eventtap.keyStrokes(" ¯\\_(ツ)_/¯") end
function snipOrgDate() hs.eventtap.keyStrokes(os.date("<%Y-%m-%d %a>")) end
function snipISODate() hs.eventtap.keyStrokes(os.date("%Y-%m-%d")) end

function menuCapitalize()
   local app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Transformations", "Capitalize"})
end

function menuUpperCase()
   local app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Transformations", "Make Upper Case"})
end

function menuLowerCase()
   local app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Transformations", "Make Lower Case"})
end

function webSearch(name, url)
   hs.focus()
   button, message = hs.dialog.textPrompt(name, "Search " .. name .. "for:", "", "Search", "Cancel")
   if button == 'Cancel' then
      return
   else
      search = hs.http.encodeForQuery(message)
      hs.urlevent.openURL(url .. search)
   end
end

function searchYouTube()   webSearch("YouTube",   "http://www.youtube.com/results?search_query=") end
function searchGitHub()    webSearch("GitHub",    "https://github.com/search?q=") end
function searchWikipedia() webSearch("Wikipedia", "https://en.wikipedia.org/w/index.php?search=") end
function searchIMDB()      webSearch("IMDB",      "https://www.imdb.com/find?q=") end
function searchWolframAlpha() webSearch("Wolfram Alpha", "https://www.wolframalpha.com/input?i=") end

function searchGoogle()
   -- This function was written by chatGTP
   -- Create a chooser for user input
   local chooser = hs.chooser.new(function(choice)
      if not choice then return end
      -- Open a URL for a Google search with the selected query
      hs.urlevent.openURL("https://www.google.com/search?q=" .. hs.http.encodeForQuery(choice.text))
      -- hs.alert(hs.http.encodeForQuery(choice.text))
   end)
   chooser:placeholderText("Google Search...")
   -- Fetch autocomplete suggestions from Google
   chooser:queryChangedCallback(function(string)
      if #string == 0 then return end
      hs.http.asyncGet("https://suggestqueries.google.com/complete/search?client=firefox&q=" .. hs.http.encodeForQuery(string), nil, function(status, body)
         if status ~= 200 then return end
         local suggestions = hs.json.decode(body)[2]
         local results = {}
         for i, suggestion in ipairs(suggestions) do
            table.insert(results, {
               ["text"] = suggestion
            })
         end
         chooser:choices(results)
      end)
   end)
   chooser:show()
end

function newFinderWindow()
   finder = hs.appfinder.appFromName("Finder")
   hs.osascript.applescript('tell application "Finder" to make new Finder window')
   finder:activate()
end

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


-- Anycomplete
-- -----------------------------------------------

anycomplete = hs.loadSpoon("Anycomplete")
anycomplete.engine = "duckduckgo"
anycomplete.bindHotkeys()


-- My Hammerspoon Menubar Item
-- ----------------------------------------------

-- Name the menubar item
local myHammerMenu = hs.menubar.new()

-- Build the actual menubar item drop-down
function myHammerMenuItem()
   local snippetMenu = {
      { title = "(waving hands around)", fn = snipWave },
      { title = " ¯\\_(ツ)_/¯", fn = snipShrug },
      { title = "<YYYY-MM-DD DDD>", fn = snipOrgDate },
      { title = "YYYY-MM-DD", fn = snipISODate },
   }
   local searchMenu = {
      { title = "Search YouTube", fn = searchYouTube },
      { title = "Search GitHub", fn = searchGitHub },
      { title = "Search Wikipedia", fn = searchWikipedia },
      { title = "Search IMDB", fn = searchIMDB },
      { title = "Search Wolfram Alpha", fn = searchWolframAlpha },
      { title = "Search Google", fn = searchGoogle },
   }
   local menuTable = {
      { title = "Toggle Dark Mode", fn = toggleDarkMode },
      { title = "Toggle Stage Manager", fn = toggleStageMan },
      { title = "Toggle Menu Bar", fn = toggleMenubar },
      { title = "-" },
      { title = "Snippets", menu = snippetMenu },
      { title = "Search...", menu = searchMenu },
      { title = "-" },
      { title = "Mail", disabled = true },
      { title = "New Mail Message", fn = newMailMessage },
      { title = "Copy Mail Message URL", fn = copyMailURL},
   }
   myHammerMenu:setMenu(menuTable)
end

-- Add the menubar item to the menubar
myHammerMenuItem()

local iconH = [[ASCII:
1.............1
7.............5
...............
...............
....AD...FI....
...............
...............
....K.....L....
....N.....M....
...............
...............
....BC...GH....
...............
...............
7.............5
3.............3
]]

myHammerMenu:setIcon(iconH)


-- Bindings
-- ----------------------------------------------

keyBindings = {
   { alpha, 'm', 'Mail' },
   { alpha, 'n', 'Notes' },
   { alpha, 'c', 'Calendar' },
   { alpha, 'b', 'BBEdit' },
   { alpha, 'e', 'Emacs' },
   { alpha, 's', 'Safari' },
   { alpha, 'a', 'Music' },
   { alpha, 't', 'Terminal' },
   { alpha, 'r', 'Reminders' },
   { alpha, 'f', newFinderWindow },
   { alpha, 'g', searchGoogle },
   -- hyper g reserved for Anycomplete
   { hyper, 't', snipISODate },
   { hyper, 's', toggleStageMan },
   { hyper, 'd', toggleDarkMode },
   { hyper, 'v', pastePlainText },
   { {'alt', 'cmd'}, 'm', toggleMenubar },
}

for i, mapping in ipairs(keyBindings) do
   local mod = mapping[1]
   local key = mapping[2]
   local app = mapping[3]
   hs.hotkey.bind(mod, key, function()
     if (type(app) == 'string') then
            hs.application.launchOrFocus(app)
     else
        app()
     end
   end)
end


-- User Keymaps
-- ----------------------------------------------
-- This creates keymaps for specific apps, and creates an application watcher
-- that activates and deactivates the mappings when the associated app
-- activates.

-- Excel Mode Map

excelModeMap = hs.hotkey.modal.new()

excelModeMap:bind({'cmd'}, 'return', function() keyUpDown({}, 'f2') end)

-- Bike Mode Map

bikeModeMap = hs.hotkey.modal.new()

bikeModeMap:bind({'alt'}, 'b', function() keyUpDown({'alt'}, 'left') end)
bikeModeMap:bind({'alt'}, 'f', function() keyUpDown({'alt'}, 'right') end)
bikeModeMap:bind({'alt'}, 'd', function() keyUpDown({'alt'}, 'forwarddelete') end)

-- Readline Mode Map

readlineModeMap = hs.hotkey.modal.new()

readlineModeMap:bind({'alt'}, 'l', function() menuLowerCase() end)
readlineModeMap:bind({'alt'}, 'c', function() menuCapitalize() end)
readlineModeMap:bind({'alt'}, 'u', function() menuUpperCase() end)


-- App Activation Watcher

function appActivation(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      if (appName == "Emacs") or (appName == "Terminal") then
         readlineModeMap:exit()
      else
         readlineModeMap:enter()
      end
      if (appName == "Microsoft Excel") then
         excelModeMap:enter()
      else
         excelModeMap:exit()
      end
      if (appName == "Bike") then
         bikeModeMap:enter()
      else
         bikeModeMap:exit()
      end
   end
end

appActivationWatcher = hs.application.watcher.new(appActivation)

appActivationWatcher:start()


-- Transient Keymap
-- ---------------------------------------------
-- This creates a custom transient keymap that is only active for one event
-- and then exists.

-- Create the model keymap to bind inside of
transientKeys = hs.hotkey.modal.new()

-- Create the menubar item
function transientKeys:entered()
   myKeysMenuItem = hs.menubar.new():setTitle("􀇳 Transient Keymap!")
   myKeysMenuItem:setTooltip("Press Escape to deactivate.")
end

-- Remove the menu item
function transientKeys:exited()
   myKeysMenuItem:delete()
end

transientKeysBindings = {
   { {}, 'n', newMailMessage },
}

do -- Set the binding and provide an escape, while preventing recursion.
   local mod = power
   local key = 'space'
   hs.hotkey.bind(mod, key, function() transientKeys:enter() end)
   transientKeys:bind(mod, key, function() transientKeys:exit() end)
   transientKeys:bind('', 'escape', function() transientKeys:exit() end)
end

for i, mapping in ipairs(transientKeysBindings) do
   local mod = mapping[1]
   local key = mapping[2]
   local fn  = mapping[3]
   transientKeys:bind(mod, key, function() fn() transientKeys:exit() end)
end


-- END HAMMERSPOON CONFIG --
