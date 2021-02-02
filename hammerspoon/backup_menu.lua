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

-- functions called by the menubar item
function backupNow () os.execute("launchctl start local.restic.test") end
function backupOpenLogs () os.execute("open ~/home/src/restic/logs") end
function backupRunning()
   backupMenu:setIcon(cloud_run:setSize({w=20,h=20}))
end
function backupSuccess()
   backupMenu:setIcon(cloud_ok:setSize({w=20,h=20}))
   backupMenuItem()
end
function backupFail()
   backupMenu:setIcon(cloud_fail:setSize({w=20,h=20}))
   backupMenuItem()
end

-- Register URLs and bind them to the above functions so that the backup
-- script can update the menu item.
hs.urlevent.bind("backup_running", backupRunning)
hs.urlevent.bind("backup_success", backupSuccess)
hs.urlevent.bind("backup_fail", backupFail)

-- Build the menu item
function backupMenuItem()
   -- First, get the LAST log file, we will use this in the menuTable
   local lastBackup = hs.execute("ls ~/home/src/restic/logs | tail -1")
   -- Generate the menu items you want in the list
   local menuTable = {
      { title = "Last Backup:", disabled = true },
      { title = lastBackup, fn = backupOpenLogs },
      { title = "-" },
      { title = "Backup Now", fn = backupNow },
   }
   backupMenu:setMenu(menuTable)
end

-- Run the function to place an item in the menubar
backupMenuItem()
backupMenu:setIcon(cloud_idle:setSize({w=20,h=20}))

