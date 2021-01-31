-- Backup Menu
-- -----------------------------------------------
-- A menu item for monitoring/controlling restic backups run by launchd.
-- The actual backup script interacts with this.

-- Create the menubar item
local backupMenu = hs.menubar.new()
-- Set the default icon, this will be replaced when the backup job succeeds/fails
local cloud_idle = hs.image.imageFromPath("assets/cloud_idle.pdf")
backupMenu:setIcon(cloud_idle:setSize({w=20,h=20}))

-- functions called by the menubar item
function backupNow () os.execute("launchctl start local.restic.test") end
function backupOpenLogs () os.execute("open ~/home/opt/restic/logs") end
function backupRunning()
   local cloud_run = hs.image.imageFromPath("assets/cloud_run.pdf")
   backupMenu:setIcon(cloud_run:setSize({w=20,h=20}))
end
function backupSuccess()
   local cloud_ok = hs.image.imageFromPath("assets/cloud_ok.pdf")
   backupMenu:setIcon(cloud_ok:setSize({w=20,h=20}))
end
function backupFail()
   local cloud_fail = hs.image.imageFromPath("assets/cloud_fail.pdf")
   backupMenu:setIcon(cloud_fail:setSize({w=20,h=20}))
end

-- Register URLs and bind them to the above functions so that the backup
-- script can update the menu item.
hs.urlevent.bind("backup_running", backupRunning)
hs.urlevent.bind("backup_success", backupSuccess)
hs.urlevent.bind("backup_fail", backupFail)

-- Build the menu item
function backupMenuItem()
   -- First, get the LAST log file, we will use this in the menuTable
   local lastBackup = hs.execute("ls ~/home/opt/restic/logs | tail -1")
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

