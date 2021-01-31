-- QuickMenu
-- -----------------------------------------------
-- This is used for misc scripts I want to occasionally run, but don't want to
-- bother remembering a keybinding for. Somewhat like my personal FastScripts.

-- Create menu bar item
local quickMenu = hs.menubar.new()

function buildQuickMenu()
   local snippetMenu = {
      { title = "waving hands around", fn = snipWave },
      { title = " ¯\\_(ツ)_/¯", fn = snipShrug },
      { title = "[Org Mode Date]", fn = snipOrgDate },
      { title = "[YYYY-MM-DD]", fn = snipISODate },
   }
   local menuTable = {
      { title = "QuickMenu", disabled = true },
      { title = "-" },
      { title = "Copy Mail Message URL", fn = copyMailURL, shortcut = "m"},
      { title = "New Mail Message", fn = newMailMessage },
      { title = "-" },
      { title = "Open Org Inbox", fn = openOrgInbox, shortcut = "i" },
      { title = "-" },
      { title = "Safari tabs → Org Inbox", fn = safariTabs2ORG },
      { title = "iOS Inbox → Org Inbox", fn = importIOSinbox },
      { title = "Clipboard → Org Inbox", fn = clipboard2ORG },
      { title = "-" },
      { title = "Snippets", menu = snippetMenu },
      { title = "Open Google Drive Folder", fn = function() fileStreamChooser:show() end },
      { title = "-" },
      { title = "Remove From Menu Bar", fn = killQuickMenu },
   }
   local utilIcon = hs.image.imageFromPath("assets/hare.pdf")
   quickMenu:setIcon(utilIcon:setSize({w=20,h=20}))
   quickMenu:setMenu(menuTable)
end


-- FUNCTIONS FOR MENUBAR --

function killQuickMenu() quickMenu:delete() end

function safariTabs2ORG()
   os.execute( "printf '\n** TODO Safari Tabs\n\n' >> ~/home/org/inbox.org && ~/home/dot/bin/safariTabs >> ~/home/org/inbox.org && open hammerspoon://success" )
end

function clipboard2ORG()
   os.execute( "printf '\n** TODO Clipboard Refile\n\n' >> ~/home/org/inbox.org && pbpaste >> ~/home/org/inbox.org && open hammerspoon://success" )
end

function copyMailURL()
   os.execute( "~/home/dot/bin/getMailURL | pbcopy | open hammerspoon://success" )
end

function openOrgInbox() os.execute("open ~/home/org/inbox.org") end
function importIOSinbox() os.execute("~/Desktop/moveiOS2ORG") end

function newMailMessage() os.execute("open mailto:") end
hs.hotkey.bind({"ctrl", "cmd", "shift"}, "m", newMailMessage)

-- SNIPPETS --
function snipWave() hs.eventtap.keyStrokes("(waving hands around)") end
function snipShrug() hs.eventtap.keyStrokes(" ¯\\_(ツ)_/¯") end
function snipOrgDate() hs.eventtap.keyStrokes(os.date("<%Y-%m-%d %a>")) end
function snipISODate() hs.eventtap.keyStrokes(os.date("%Y-%m-%d")) end

-- Finally, build the menubar item
buildQuickMenu()

