---
editor:
  markdown:
    wrap: 250
    canonical: true
---

-   Location: [ADDRESS](GOOGLE%20LINK)
-   Date: {{course_date}}
-   Pre-course assignments dates (\~3-5 hrs to complete, suggest they dedicate a full day to it):
    -   Start: {{tasks_start_date}}
    -   Due: {{tasks_check_end_date}}

## Instructors

Instructors/helpers:

Before the course, please review the CONTRIBUTING guide, Code of Conduct, and the Tips for instructors or helpers in the [Guides](https://guides.rostools.org/) website.

{{session_schedule_table}}

## To do before course

Several of the below tasks (for the admin/lead instructor) can be done using the r3admin package.

-   [ ] {{tasks_start_date}}: (Optional) Send an calendar invite to registered participants with link to pre-course tasks and website.
-   [ ] {{instructors_precourse_meeting}}: Send a calendar invite to instructors/helpers for course location (with planning issue link), dinner locations, and post-course debrief.
    -   Include registered participants with link to pre-course tasks and website.
-   [ ] \~{{instructors_precourse_meeting}}: Pre-course instructor meeting:
    -   Go briefly over [Instructor](https://guides.rostools.org/instructors.html) and [Helper](https://guides.rostools.org/helpers.html) with new instructors.
    -   Mention about post-course debrief (described in the [Guides](https://github.com/github/rest-api-description) site).
    -   (For intro course) Have all instructors/helpers give their GitHub user names.
-   [ ] {{tasks_remind_date}}: Send a reminder to everyone to complete tasks.
-   [ ] {{tasks_check_end_date}}: Check that all participants have completed pre-course material.
-   [ ] {{tasks_prep_end_date}}: Get participants into groups (\~3-4 people per group).
-   [ ] {{tasks_prep_end_date}}: Assign instructors to groups.
-   [ ] {{tasks_prep_end_date}}: Open up the Google Forms feedback survey

## To do during course

-   [ ] Set up repo to host code-along that instructors will use (course dependent).

## To do after course

-   [ ] Close the Google Forms feedback survey
-   [ ] Update git tag and release it (`r3admin::create_course_tag()`)
-   [ ] Update course metadata in `r3admin/data/courses.yaml`
-   [ ] Update instructor/helper metadata in `r3admin/data/courses.yaml` and `r3admin/data/people/`
-   [ ] Update Zenodo DOI

## Other details

-   DDA will reimburse travel expenses [here](https://www.ddeacademy.dk/content/online-compensation-form-events)
