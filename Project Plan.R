# Updated punchlist after kickoff meeting 28/09/21 ----
#    [y] Indicate CL and SL lines on all charts (pull specs from LIMS)
#    [y] Finalise visual style of charts (colours, line weights, etc)
#    [y] Suitability of all 4 rules (e.g. remove rules 3 & 4 on gran/gram)
#    [y] Data on amount of floss in containers loaded out
#    [y] Create a summary/home page with 'traffic lights'
#    [y] Update 'floss' code to use bulk silos instead of BLPs
#    [y] Seperate out DB queries and just do filters reactively
#    [y] Finalise Look and Layout of Floss charts
#    [y] Add 'conditions' for dynamically updating traffic lights
#    [y] Common date breaks for floss and other charts (just added it into 'FLOSS' function)
#    [y] Add handling for when no Ash results present
#    [y] Finalise the header texts
#    [y] Optimise performance by using reactive programming correctly?

# Updated punchlist 24/09/21 ----

#    [y] Test variable date ranges on existing SPC function
#    [y] Create a specification table for reference (.png image?)
#    [y] Create a test Shiny app with desired layout and dummy ggplots
#    [y] Test reactive plots in Shiny - do we need this?
#    [y] Demonstrate static version of the web app and get buy-in!
#    [y] Update Shiny app to have a slider with no. days (and reactive plot updates)


# There will be several facets to this project, each can be completed individually then integrated:----

#    1) LIMS Database Query
#        * Already have existing codebase for how to do this
#        * Will be an issue with having 'live' access to the database when not on the Qenos network
#        * This means the final app would have to run on a locally networked server
#        * Meantime, I could just download a big chunk of the data and run it locally


#    2) Control Charts
#        * Should be able to set something up using qicharts package (cancelled)
#        * Should limit control chart time scale to a week or so
#        * Will need to account for different grades
#        * Focus should be on Density, Swell Ratio and Good Cuts for each RV
#        * Need to select appropriate rules for 'out of control' points


#    3) Web App Development
#        * Have separate 'tabs' for each RV?
#        * UI can be pretty basic
#        * Hopefully HTML aspects are not too hard... ask Kurt for help


#    4) Server Setup
#        * Initially host this on VM at home
#        * Need to just test 'proof of concept'
#        * Actual implementation will require IT support
#        * Need to have a demo to show in order to get buy-in about its value