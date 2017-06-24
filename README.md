# Shiny Survey

A survey made in Shiny, designed to collect data and save it into a Dropbox account.

Made using the guidance outlined in the "Persistent data storage in Shiny Apps" post by Dean Attali. https://shiny.rstudio.com/articles/persistent-data-storage.html

## Background

The students that we work with can cycle in and out of partner services, sometimes on a weekly basis, and sometimes only up to an hour before the session starts. 

We needed a data collection tool that could reflect and automatically update the list of students that partners see, depending on the date of the session.

We were previously relying on Lime Survey (an open source survey tool with similar functionality to Survey Monkey), but we found this system was not responsive enough, as it required someone to manually go into the backend of the program and update the list of students.

Instead, we needed a tool that could reflect and automatically update the list of students that partners see, depending on the date of the session. 

## Functionality

A separate R sript pulls a list of students, partners, and session dates from SalesForce.

Partners enter their unique password and date of the session, which are used as filters for the list of students that partners see. For partners running multiple sessions across schools, days, or volunteers, additional filters are used.

Partners enter data about the attendance of students at partner services. For those students that attended, they are also required to enter data about how engaged they felt the student was in the session.

