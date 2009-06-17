OPTIONS NOCENTER NODATE NONUMBER LINESIZE=120 PAGESIZE=32627 compress=yes;
libname ahsraw 'd:\AHS data\metro';
libname rawdata xport 'd:\AHS data\metro\HUD.XPT';
  * Note: Lines above are where the input and output files are located.;
  * They should be changed to reflect your local installation;

/*
* Name: In_ahs_ms.sas
  Date: 12/28/1999
  This is an experimental program designed to convert the AHS METRO SURVEY
  SAS transport files provided by the Census Bureau into a single SAS dataset with
  a similar structure to pre-1997 data.  It is based on the in_ahs97.sas program that
  Gregory Watson of ICF Consulting wrote for the 1997 AHS national files.

  The modifications were performed by:

  David A. Vandenbroucke
  Economist, Policy Development and Research
  U.S. Department of Housing and Urban Development
  451 7th Street SW, Room 8218
  Washington, DC    20410

  phone:  202-708-1060 ext. 5890
  fax:    202-708-3316
  david_a._vandenbroucke@hud.gov

  COMMENTS FOR IN_AHS_MS.SAS
  The main differences between this program and IN_AHS97.SAS are:
     * Changed datasets:  No HOMIMP, JTW.  Added VRATIO
     * Changed sorting and BY statments to sort by SMSA and CONTROL


  COMMENTS FOR IN_AHS97.SAS:
  This program is a cleaned up version of Readraw14.sas, with
  with the version history and unnecessary code removed.

  The program was developed under Task Order 14 under HC-5966 by
  the ICF Consulting Group, part of ICF Kaiser International, under
  contract with HUD.

  For questions about the SAS program, please contact:
    Gregory Watson
    Senior Associate
    Housing and Community Development
    ICF Incorporated
    9300 Lee Highway
    Fairfax, VA 22031
    Phone: 703-934-3694
    Fax:   703-934-3156
    Email: gwatson@icfkaiser.com

  Questions to HUD about the AHS should be directed to either:
    Dav Vandenbrouke (email: david_a._vandenbroucke@hud.gov) or
    Ron Sepanik (ronald_j._sepanik@hud.gov).
    They can both be reached by phone at: 202-708-1060.

  Questions to Census about the AHS should be directed to either:
    Barbara Williams (email: Barbara_T._Williams@ccmail.census.gov) or
    Paul Harple (email: paul_pearson_harple_jr@ccmail.census.gov)
    They can both be reached by phone at: 301-457-3235.
*/

* First extract the individual files from the SAS transport file;

proc copy in=rawdata out=ahsraw;

* End of reading from the raw files;


* Define a macro to show the contents of each individual file,
  and set a flag.;

%macro runcim(filen);

proc contents data=ahsraw.&filen;
  title "&filen file";

proc sort data=ahsraw.&filen;
  by SMSA control;

data ahsraw.&filen;
  set ahsraw.&filen;
  &filen.f=1;

%mend;

* Run the macro for each individual file;
/*
     This section changed to reflect the different files in the metro
     dataset.
*/

%runcim(houshld);
%runcim(mortg);
%runcim(owner);
%runcim(person);
%runcim(ratiov);
%runcim(rmov);
%runcim(toppuf);
%runcim(weight);

* Set up the individual data to handle multiple observations for the
  same household;

* Multiple observations in the person file.;

data ahsraw.person;
  set ahsraw.person;
  if rel=1 or rel=2 then hhead=1;
    else hhead=0;
  movgrp=mvg;
  famgrp=mvg;

  * Lines above also prepare for other merging.;

proc sort data=ahsraw.person;
  by SMSA control;

proc means noprint data=ahsraw.person;
  var age;
  by SMSA control;
  output out=junk n=numfam;

data junk;
  set junk;
  keep SMSA control numfam;

data ahsraw.person;
  merge ahsraw.person junk;
  by SMSA control;

* Merge on JTW here;
* this section deleted because there is no jtw in the metro;

* Merge in Recent Movers here;

proc sort data=ahsraw.person;
  by SMSA control movgrp;

proc sort data=ahsraw.rmov;
  by SMSA control movgrp;

data ahsraw.person;
  merge ahsraw.person ahsraw.rmov;
  by SMSA control movgrp;

proc sort data=ahsraw.person;
  by SMSA control hhead descending pline;

* Multiple observations in the Home Improvement file;
* this section deleted because there is no homimp in metro dataset;

* Check to make sure no duplicate weights;

data ahsraw.weight;
  set ahsraw.weight;
  by SMSA control;
  if first.control;

* Define a macro to handle the change in format to be
  similar to the old style where there is a single household
  record as opposed to individual records;

%macro lagvar(newvar,origvar,arvar);
  &newvar.1=&origvar;
  &newvar.2=lag1(&origvar);
  &newvar.3=lag2(&origvar);
  &newvar.4=lag3(&origvar);
  &newvar.5=lag4(&origvar);
  &newvar.6=lag5(&origvar);
  &newvar.7=lag6(&origvar);
  &newvar.8=lag7(&origvar);
  &newvar.9=lag8(&origvar);
  &newvar.10=lag9(&origvar);
  &newvar.11=lag10(&origvar);
  &newvar.12=lag11(&origvar);
  &newvar.13=lag12(&origvar);
  &newvar.14=lag13(&origvar);
  &newvar.15=lag14(&origvar);
  &newvar.16=lag15(&origvar);
  array &arvar {*} &newvar.1 &newvar.2-&newvar.16;
  do x=1 to 16;
    if x>numfam then &arvar(x)=.;
  end;
  drop x &origvar;
%mend;

* Now set up code to do transformation to single record from
  multiple record.;

* Transform person file;

data ahsraw.person;
  set ahsraw.person;
  by SMSA control hhead descending pline;

  %lagvar(age,age,tempa);
  employ=remp;
  %lagvar(employ,employ,tempk);
  %lagvar(grad,grad,tempb);
  %lagvar(move,move,tempc);
  movyr=move;
  %lagvar(movyr,movyr,tempd);
  %lagvar(movm,movm,tempdz);
  %lagvar(par,par,tempe);
  %lagvar(pline,pline,tempf);
  %lagvar(rel,rel,tempg);
  %lagvar(rntdue,rntdue,temph);
  %lagvar(sal,sal,tempi);
  %lagvar(spos,spos,tempj);
  famgrp=mvg;
  %lagvar(famgrp,famgrp,templ);
  %lagvar(food,food,tempm);
  %lagvar(here,here,tempn);
  %lagvar(hhfood,hhfood,tempo);
  * %lagvar(hhmem,hhmem,tempp);
  %lagvar(hhmort,hhmort,tempq);
  %lagvar(hhoth,hhoth,tempr);
  %lagvar(hhutfu,hhutfu,temps);
  %lagvar(mar,mar,tempt);
  %lagvar(othest,othest,tempu);
  %lagvar(race,race,tempv);
  %lagvar(sex,sex,tempw);
  %lagvar(span,span,tempx);
  %lagvar(ten,ten,tempy);
  lodstat=lodsta;
  %lagvar(lodsta,lodstat,tempz);
  %lagvar(lodrnt,lodrnt,tempaa);
  * %lagvar(wlineq,wlineq,tempjt);
  * Line above moved to JTW;
  %lagvar(famnum,famnum,tempfa);
  %lagvar(famrel,famrel,tempfb);
  %lagvar(famtyp,famtyp,tempfc);

  * Do transform for JTW portion;
  * deleted because metro has no jtw;

  * Start of special code to transform movers;

  %lagvar(movgrp,movgrp,tempat);
  %lagvar(xcond,xcond,tempau);
  %lagvar(xcoop,xcoop,tempav);
  %lagvar(xcost,xcost,tempaw);
  %lagvar(xhead,xhead,tempax);
  %lagvar(xinus,xinus,tempay);
  %lagvar(xper,xper,tempaz);
  %lagvar(xrel,xrel,tempba);
  %lagvar(xten,xten,tempbb);
  %lagvar(xunit,xunit,tempbc);

  if last.control=1 then output;
  drop hhead;

* Transform home improvement file;
* deleted because there is no homip in metro;

* Merge all files together;
* RMOV already merged into person file;

data ahsraw.ahs98MS;
  merge ahsraw.owner ahsraw.houshld ahsraw.mortg
        ahsraw.person ahsraw.toppuf ahsraw.ratiov ahsraw.weight;
  by SMSA control;
  * Delete cases where we are either missing a weight, or have only the weight;
  if ownerf=. and houshldf=. and mortgf=. and ratiovf=. and
     personf=. and rmovf=. and toppuff=. and weightf=1 then delete;
  * If weight flag is missing, assign 0 weight, should not occur.;
  if weightf=. then weight=0;

proc datasets ddname=ahsraw;
  delete owner houshld mortg person rmov toppuf ratiov weight;
run;

proc freq noprint data=ahsraw.ahs98MS;
  table control / out=gjw;
  by SMSA;

proc freq data=gjw;
  table SMSA;
  title "AHS98 MS File";
  title2 "Count of number of observations by metro area";

proc contents data=ahsraw.ahs98MS;
  title "Contents of AHS 98 Metro file";

run;
