/*

This query essentially generates the dataset for Activation Model.

Tables useds:

                                Salescenter table:


select *

from (

 

 

 

 

/*

In the following block we query salescenter tables and get the first_ticket(first time sale made by a rep) column.

We are creating a view with name sc.

We need two salescenter tables for this: 

this table gives us the sale dates of reps by brokerid

this table gives us the npn number of the reps

*/

 

with sc as 

(select

                b.npn_number,

                min(a.start_date) as first_ticket,

                max(a.start_date) as latest_ticket,

                COUNT(a.policyno) as num_tickets,

                sum(a.transamount) as tot_premium

                from a

                               

                                inner join b

                                                on a.brokerid = b.brokerid

 

                where  finantype = 'I'

                and transcode not in ('S','W','L')

                and case_ind = 1

                group by npn_number)

               

                /*by using where totalcomm > 0 we reduce the number of duplicate policies by 90%*/

                /*when we remove AXA Advisors from the pool it takes away another 5% of the duplicate policies*/

               

                select

               

/*

Initially we list the variable from both discovery table.

*/

 

 

ins.npn,

ins.number_yearsanagent,

ins.securitiesregisteredagent,

ins.brokerdealeraffiliation,

ins.earliestappointmentdate,

ins.carrierappointments,

ins.gender,

ins.agentlicensetype_health,

ins.number_statelicenses_health,

ins.agentlicensetype_life,

ins.number_statelicenses_life,

ins.agentlicensetype_propertycasualty,

ins.number_statelicenses_propertycasualty,

ins.agentlicensetype_variableproducts,

ins.number_statelicenses_variableproducts,

ins.firmname,

ins.riaaffiliation,

ins.becameagentdate,

ins.monthid,

ins.is_in_insurance_table,

rbd.discoverycontactid,

rbd.repcrd,

rbd.firstname,

rbd.lastname,

rbd.suffix,

rbd.title,

rbd.titlecategories,

rbd.bdfirmcrd,

rbd.bdfirmname,

rbd.branch_zipcode,

rbd.branch_zipcode4,

rbd.branch_addressupdate,

rbd.number_branchreps,

rbd.email_businesstype,

rbd.email_businesstypevalidationsupported,

rbd.email_businesstypeupdate,

rbd.email_business2type,

rbd.email_business2typevalidationsupported,

rbd.email_business2typeupdate,

rbd.dateaddedtodiscoverydata,

rbd.datebecamerep_numberofyears,

rbd.dateofhireatcurrentfirm_numberofyears,

rbd.dateofbirth_full,

rbd.home_zipcode,

rbd.home_zipcode4,

rbd.home_addressupdate,

rbd.successlikelihood,

rbd.nonproducer,

rbd.independentcontractor,

rbd.duallylicensedbdriarep,

rbd.duallyregisteredbdriarep,

rbd.possiblefundinvestingrep,

rbd.insurancelicensed,

rbd.sellsretirementplanproducts,

rbd.number_registeredstates,

rbd.worksathqoffice,

rbd.licensesdesignations,

rbd.designations_cfa,

rbd.designations_cfp,

rbd.designations_cpa,

rbd.designations_chfc,

rbd.designations_clu,

rbd.priorfirm1_firmcrd,

rbd.priorfirm1_numberofyears,

rbd.priorfirm2_firmcrd,

rbd.priorfirm2_numberofyears,

rbd.priorfirm3_firmcrd,

rbd.priorfirm3_numberofyears,

rbd.branch_discoveryaddressid,

fbd.number_firmreps,

is_in_bdrep_table,

                first_ticket,

                latest_ticket,

                num_tickets,

                tot_premium,

               

/*

We create a custom variables from discovery dataset.

title_simple : indicator if titlecategories contains 'advisor'

is_variable_licensed: indicates if the rep has license for variable products

has_license_to_sell: if the rep licenses (6 or 7) and (63 or 66)

*/

 

 

 

  case when rbd.titlecategories like '%Advisor%' then "advisor" else 'others' end as title_simple,

  case when ins.agentlicensetype_variableproducts = 'Yes' then 1 else 0 end as is_variable_licensed,

  case when rbd.titlecategories like '%Advisor%' then 1 else 0 end title_advisor,

  case when (rbd.licensesdesignations like '%6 %' or rbd.licensesdesignations like'%7 %') and

      (rbd.licensesdesignations like '%63%' or rbd.licensesdesignations  like '%66%') then 1 else 0 end as has_license_to_sell,

 

                  /*possible titlecategories (100% complete) are Advisor, Bank Advisor, Call Center Advisor,

Administration, Advisor Assistant, Branch Manager, Branch AdminOps,

Compliance/Legal, Executive, Finance/Accounting, Investment Banking,

Operations/Technology, Owner, Planning Specialist, Research, Research Director,

Retirement Plan Specialist, Sales/Marketing, Trading Desk, Trust Officer, Wholesaler,

Other and Unknown.*/

 

  

  

  /* We now list variables from firm_friendliness, branch_friendliness, and market metrics tables*/

 

  fma.channeltype,

  fma.firmcode,

  mm.pene_rate as firm_zip3_depth_mm,

 

  /* Now we create couple of custom variables

                                counter:  define the scope of our case

                                                # counter = 0, if the rep has never given a ticket and the rep has licenses to sell IA products

                                                # counter = 1, if the first ticket is dropped between Sep 30, 2015 and Sep 30, 2017

                                                # counter = 2, if the rep has first ticket before Sep 30, 2015 and has sold a ticket since Sep 30, 2015

                                title_advisor_unknown: indicator if titlecategories contains 'advisor' or 'unknown'

  */

 

  

      case

      when first_ticket is null and is_in_bdrep_table = 1 and ins.agentlicensetype_variableproducts = "Yes" then 0

      when first_ticket between '2015-09-30' and '2017-09-30' then 1

      when first_ticket < '2015-09-30' and latest_ticket > '2015-09-30' then 2 end as counter,

    

    case when (titlecategories like '%Advisor%') or

        (lower(titlecategories) like '%unknown%' and lower(independentcontractor) like '%yes%' ) then 1 else 0 end as title_advisor_unknown

                 

                  from

   

     /*

    We join both of the discovery tables and Number_BDReps from bd_firm first.

    */

   

    

     (select *,substr(ss_date,1,7) as monthid,1 as is_in_insurance_table from sfsf)ins

      left join

        (select *,substr(ss_date,1,7) as monthid, 1 as is_in_bdrep_table from ) rbd

               on ins.repcrd = rbd.repcrd

               and ins.monthid = rbd.monthid

                                               

                                                left join fbd

                                                on regexp_extract(rbd.bdfirmcrd, '[0-9]+', 0) = fbd.bdfirmcrd      

                                               

                                                    /*

     We now merge the salecenter data

     */     

                                                        left join sc

               on regexp_extract(ins.npn, '[0-9]+', 0) = sc.npn_number

 

                /*

     We now merge the discovery_sc_firm_match table. this is a one to one mapping of the firm code in salescenter to firm code in discovery.

     */ 

        

        left join us_ia_3_usecase.discovery_sc_firm_match fma

               on regexp_extract(rbd.bdfirmcrd, '[0-9]+', 0) = fma.firmcrd                                          

               

                                                /*

We now merge the market metrics data to this dataset, on branchcode,3 digit zipcode and monthid

        

        

        */

       

        left join (select mm.*

                from

                    (select firmcode

                    ,substr(zip_code,1,3) as zip3

                    ,quarter

                    ,sum(cast(client_sales as double))/sum(cast(global_sales as double)) as pene_rate

                    from (

                        select a.*,b.firmcode from  a

                        left join (select fdsbrokerid,brokerid as firmcode from 
                        group by 1,2) b

                        on a.fds_broker_id = b.fdsbrokerid

                    ) qwe

                    where quarter = 'Q3 2017'

                    group by 1,2,3) mm) mm

                on fma.firmcode = mm.firmcode

                               and rbd.branch_zipcode3digitsectional = mm.zip3

                                                ) entire_data

                                                where  counter in (0, 1, 2)

                                                and monthid = '2017-09'

;
