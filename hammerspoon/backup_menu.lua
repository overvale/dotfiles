-- Backup Menu
-- -----------------------------------------------
-- A menu item for monitoring/controlling restic backups run by launchd.
-- The actual backup script interacts with this.

-- Create the menubar item
local backupMenu = hs.menubar.new()

-- Set the icons for the menu bar item
local cloud_idle = hs.image.imageFromPath("assets/cloud_idle.pdf")
local cloud_run  = hs.image.imageFromPath("assets/cloud_run.pdf")
local cloud_ok   = hs.image.imageFromPath("assets/cloud_ok.pdf")
local cloud_fail = hs.image.imageFromPath("assets/cloud_fail.pdf")
local cloud_offline = hs.image.imageFromPath("assets/cloud_offline.pdf")

-- functions called by the menubar item
function backupNow () os.execute("launchctl start local.restic.test") end
function backupOpenLogs () os.execute("open ~/home/src/restic/logs") end
function openLastBackupLog () os.execute("open \"$(find ~/home/src/restic/logs | sort | tail -1)\"") end
function backupRunning()
   local frontWindow = hs.window.frontmostWindow()
   backupMenu:setIcon(cloud_run:setSize({w=20,h=20}))
   frontWindow:focus()
end
function backupSuccess()
   local frontWindow = hs.window.frontmostWindow()
   backupMenu:setIcon(cloud_ok:setSize({w=20,h=20}))
   backupMenuItem()
   frontWindow:focus()
end
function backupFail()
   local frontWindow = hs.window.frontmostWindow()
   backupMenu:setIcon(cloud_fail:setSize({w=20,h=20}))
   backupMenuItem()
   frontWindow:focus()
end
function backupOffline()
   local frontWindow = hs.window.frontmostWindow()
   backupMenu:setIcon(cloud_offline:setSize({w=20,h=20}))
   backupMenuItem()
   frontWindow:focus()
end

-- Register URLs and bind them to the above functions so that the backup
-- script can update the menu item.
hs.urlevent.bind("backup_running", backupRunning)
hs.urlevent.bind("backup_success", backupSuccess)
hs.urlevent.bind("backup_fail", backupFail)
hs.urlevent.bind("backup_offline", backupOffline)

-- Build the menu item
function backupMenuItem()
   -- First, get the LAST log file, we will use this in the menuTable
   local lastBackup = hs.execute("ls ~/home/src/restic/logs | sort | tail -1")
   -- Generate the menu items you want in the list
   local menuTable = {
      { title = "Last Backup:", disabled = true },
      { title = lastBackup, fn = openLastBackupLog },
      { title = "-" },
      { title = "Open Logs", fn = backupOpenLogs },
      { title = "Backup Now", fn = backupNow },
   }
   backupMenu:setMenu(menuTable)
end

-- Run the function to place an item in the menubar
backupMenuItem()

-- Determine the status of the last backup
lastBackupStatus = hs.execute("ls ~/home/src/restic/logs | tail -1 | cut -d ' ' -f2 | tr -d '\n'")

-- Set icon according to last backup status
if lastBackupStatus == "OFFLINE" then
   backupMenu:setIcon(cloud_offline:setSize({w=20,h=20}))
elseif lastBackupStatus == "SUCCESS" then
   backupMenu:setIcon(cloud_ok:setSize({w=20,h=20}))
elseif lastBackupStatus == "FAIL" then
   backupMenu:setIcon(cloud_fail:setSize({w=20,h=20}))
else
   backupMenu:setIcon(cloud_idle:setSize({w=20,h=20}))
end
