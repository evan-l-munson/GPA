# Gitlab Project Analyzer (GPA) <img src="www/hex_GPA.png" align="right" width="120" height="120" />

[![lifecycle](https://img.shields.io/badge/Lifecycle-Experimental-orange.svg?style=flat-square)](https://gitlab.devforce.disa.mil/netc-dsd/GPA)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-4.1.0-blue.svg?style=flat-square)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Report%20Version-0.0.8-orange.svg?style=flat-square)](https://gitlab.devforce.disa.mil/netc-dsd/GPA)
[![MIT license](https://img.shields.io/badge/License-MIT-blue.svg?style=flat-square)](http://perso.crans.org/besson/LICENSE.html)

## Purpose

Investigate the usage of DSD GitLab Code Repositories on DevForce GitLab.  The GPA report displays metrics only for the __master branch__ of a project, other branches will not register until they are merged back onto the master.

The overall goals of this project are to:
* Enable DSD Leadership to better understand the usage of our code repositories.

The application has three viewing tabs:
* Overview: Where a user can see all users and deployed content in DevForce GitLab.
* Selection: Where a user can select a specific Organization down to a single Sub-Group and see multiple metrics for that Organization.
* Project:  Where a user can filter through Groups, Sub-Groups, Sub-Sub-Groups down to a specific Project to see multiple metrics for that specified Project.
* Collaboration:  Where a user can Groups, Sub-Groups, Sub-Sub-Groups and then observe all the personnel that have committed and collaborated together on a project.

## Data

This report accesses the DevForce GitLab API to pull the applicable information.  The report uses the `gitlabr` package to connect to the API.

## References

The following references and code were used for this project.

* [`gitlabr`](https://github.com/jirkalewandowski/gitlabr)
* [GitLab API Documents](https://docs.gitlab.com/ee/api/)
