V28 :0x4 mosart_physics_mod
22 MOSART_physics_mod.F90 S622 0
04/03/2018  16:59:19
use mosart_type_mod private
use shr_const_mod private
use shr_kind_mod private
enduse
S 622 24 0 0 0 6 1 0 5031 10005 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 mosart_physics_mod
S 624 23 0 0 0 8 641 622 5063 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 r8
S 626 23 0 0 0 8 648 622 5078 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 shr_kind_cl
S 628 23 0 0 0 8 692 622 5104 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 shr_const_rearth
S 629 23 0 0 0 8 688 622 5121 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 shr_const_pi
S 631 23 0 0 0 8 1317 622 5150 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 tctl
S 633 23 0 0 0 8 1318 622 5163 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 tunit
S 634 23 0 0 0 6 1319 622 5169 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 liqwater
S 635 23 0 0 0 8 1320 622 5178 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 para
R 641 16 1 shr_kind_mod shr_kind_r8
R 648 16 8 shr_kind_mod shr_kind_cl
R 688 16 2 shr_const_mod shr_const_pi
R 692 16 6 shr_const_mod shr_const_rearth
R 1317 6 581 mosart_type_mod ctlsubw
R 1318 6 582 mosart_type_mod tunit
R 1319 6 583 mosart_type_mod liqwater
R 1320 6 584 mosart_type_mod para
S 1327 16 0 0 0 9 1 622 10433 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1328 816 0 0 0 0 0 0 0 0 0 0 0 0 0 622 0 0 0 0 tinyvalue
S 1328 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 898494074 1255454751 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 1329 23 5 0 0 0 1330 622 10454 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 euler
S 1330 14 5 0 0 0 1 1329 10454 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 22 0 622 0 0 0 0 euler
F 1330 0
S 1331 23 5 0 0 0 1334 622 10460 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hillsloperouting
S 1332 1 3 1 0 6 1 1331 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1333 1 3 1 0 9 1 1331 10483 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 thedeltat
S 1334 14 5 0 0 0 1 1331 10460 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 3 2 0 0 0 0 0 0 0 0 0 0 0 0 89 0 622 0 0 0 0 hillsloperouting
F 1334 2 1332 1333
S 1335 23 5 0 0 0 1338 622 10493 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 subnetworkrouting
S 1336 1 3 1 0 6 1 1335 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1337 1 3 1 0 9 1 1335 10483 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 thedeltat
S 1338 14 5 0 0 0 1 1335 10493 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 6 2 0 0 0 0 0 0 0 0 0 0 0 0 105 0 622 0 0 0 0 subnetworkrouting
F 1338 2 1336 1337
S 1339 23 5 0 0 0 1342 622 10511 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mainchannelrouting
S 1340 1 3 1 0 6 1 1339 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1341 1 3 1 0 9 1 1339 10483 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 thedeltat
S 1342 14 5 0 0 0 1 1339 10511 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 9 2 0 0 0 0 0 0 0 0 0 0 0 0 128 0 622 0 0 0 0 mainchannelrouting
F 1342 2 1340 1341
S 1343 23 5 0 0 0 1346 622 10530 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 routing_kw
S 1344 1 3 1 0 6 1 1343 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1345 1 3 1 0 9 1 1343 10483 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 thedeltat
S 1346 14 5 0 0 0 1 1343 10530 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 12 2 0 0 0 0 0 0 0 0 0 0 0 0 148 0 622 0 0 0 0 routing_kw
F 1346 2 1344 1345
S 1347 23 5 0 0 0 1350 622 10541 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 routing_mc
S 1348 1 3 1 0 6 1 1347 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1349 1 3 1 0 9 1 1347 10483 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 thedeltat
S 1350 14 5 0 0 0 1 1347 10541 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 15 2 0 0 0 0 0 0 0 0 0 0 0 0 183 0 622 0 0 0 0 routing_mc
F 1350 2 1348 1349
S 1351 23 5 0 0 0 1354 622 10552 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 routing_threw
S 1352 1 3 1 0 6 1 1351 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1353 1 3 1 0 9 1 1351 10483 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 thedeltat
S 1354 14 5 0 0 0 1 1351 10552 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 18 2 0 0 0 0 0 0 0 0 0 0 0 0 191 0 622 0 0 0 0 routing_threw
F 1354 2 1352 1353
S 1355 23 5 0 0 0 1358 622 10566 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 routing_dw
S 1356 1 3 1 0 6 1 1355 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1357 1 3 1 0 9 1 1355 10483 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 thedeltat
S 1358 14 5 0 0 0 1 1355 10566 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 21 2 0 0 0 0 0 0 0 0 0 0 0 0 199 0 622 0 0 0 0 routing_dw
F 1358 2 1356 1357
S 1359 23 5 0 0 0 1361 622 10577 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 updatestate_hillslope
S 1360 1 3 1 0 6 1 1359 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1361 14 5 0 0 0 1 1359 10577 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 24 1 0 0 0 0 0 0 0 0 0 0 0 0 207 0 622 0 0 0 0 updatestate_hillslope
F 1361 1 1360
S 1362 23 5 0 0 0 1364 622 10599 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 updatestate_subnetwork
S 1363 1 3 1 0 6 1 1362 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1364 14 5 0 0 0 1 1362 10599 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 26 1 0 0 0 0 0 0 0 0 0 0 0 0 214 0 622 0 0 0 0 updatestate_subnetwork
F 1364 1 1363
S 1365 23 5 0 0 0 1367 622 10622 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 updatestate_mainchannel
S 1366 1 3 1 0 6 1 1365 10477 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iunit
S 1367 14 5 0 0 0 1 1365 10622 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 28 1 0 0 0 0 0 0 0 0 0 0 0 0 231 0 622 0 0 0 0 updatestate_mainchannel
F 1367 1 1366
S 1368 23 5 0 0 0 1369 622 10646 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 subtimestep
S 1369 14 5 0 0 0 1 1368 10646 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 30 0 0 0 0 0 0 0 0 0 0 0 0 0 249 0 622 0 0 0 0 subtimestep
F 1369 0
S 1370 23 5 0 0 8 1375 622 10658 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 crvrman
S 1371 1 3 1 0 9 1 1370 10666 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 slp_
S 1372 1 3 1 0 9 1 1370 10671 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 n_
S 1373 1 3 1 0 9 1 1370 10674 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rr_
S 1374 1 3 2 0 9 1 1370 10678 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 v_
S 1375 14 5 0 0 9 1 1370 10658 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 31 3 0 0 1374 0 0 0 0 0 0 0 0 0 291 0 622 0 0 0 0 crvrman
F 1375 3 1371 1372 1373
S 1376 23 5 0 0 8 1382 622 10681 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 creht
S 1377 1 3 1 0 9 1 1376 10687 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hslp_
S 1378 1 3 1 0 9 1 1376 10693 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nh_
S 1379 1 3 1 0 9 1 1376 10697 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gxr_
S 1380 1 3 1 0 9 1 1376 10702 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 yh_
S 1381 1 3 2 0 9 1 1376 10706 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 eht_
S 1382 14 5 0 0 9 1 1376 10681 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 35 4 0 0 1381 0 0 0 0 0 0 0 0 0 307 0 622 0 0 0 0 creht
F 1382 4 1377 1378 1379 1380
S 1383 23 5 0 0 8 1387 622 10711 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 grmr
S 1384 1 3 1 0 9 1 1383 10716 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 wr_
S 1385 1 3 1 0 9 1 1383 10720 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rlen_
S 1386 1 3 2 0 9 1 1383 10726 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mr_
S 1387 14 5 0 0 9 1 1383 10711 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 40 2 0 0 1386 0 0 0 0 0 0 0 0 0 319 0 622 0 0 0 0 grmr
F 1387 2 1384 1385
S 1388 23 5 0 0 8 1392 622 10730 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 grht
S 1389 1 3 1 0 9 1 1388 10735 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mt_
S 1390 1 3 1 0 9 1 1388 10739 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 twid_
S 1391 1 3 2 0 9 1 1388 10745 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ht_
S 1392 14 5 0 0 9 1 1388 10730 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 43 2 0 0 1391 0 0 0 0 0 0 0 0 0 329 0 622 0 0 0 0 grht
F 1392 2 1389 1390
S 1393 23 5 0 0 8 1397 622 10749 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 grpt
S 1394 1 3 1 0 9 1 1393 10754 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ht_
S 1395 1 3 1 0 9 1 1393 10739 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 twid_
S 1396 1 3 2 0 9 1 1393 10758 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pt_
S 1397 14 5 0 0 9 1 1393 10749 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 46 2 0 0 1396 0 0 0 0 0 0 0 0 0 343 0 622 0 0 0 0 grpt
F 1397 2 1394 1395
S 1398 23 5 0 0 8 1402 622 10762 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 grrr
S 1399 1 3 1 0 9 1 1398 10767 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mr_
S 1400 1 3 1 0 9 1 1398 10771 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pr_
S 1401 1 3 2 0 9 1 1398 10674 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rr_
S 1402 14 5 0 0 9 1 1398 10762 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 49 2 0 0 1401 0 0 0 0 0 0 0 0 0 357 0 622 0 0 0 0 grrr
F 1402 2 1399 1400
S 1403 23 5 0 0 8 1409 622 10775 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 grhr
S 1404 1 3 1 0 9 1 1403 10767 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mr_
S 1405 1 3 1 0 9 1 1403 10780 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rwidth_
S 1406 1 3 1 0 9 1 1403 10788 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rwidth0_
S 1407 1 3 1 0 9 1 1403 10797 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rdepth_
S 1408 1 3 2 0 9 1 1403 10805 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hr_
S 1409 14 5 0 0 9 1 1403 10775 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 52 4 0 0 1408 0 0 0 0 0 0 0 0 0 371 0 622 0 0 0 0 grhr
F 1409 4 1404 1405 1406 1407
S 1410 23 5 0 0 8 1416 622 10809 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 grpr
S 1411 1 3 1 0 9 1 1410 10814 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hr_
S 1412 1 3 1 0 9 1 1410 10780 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rwidth_
S 1413 1 3 1 0 9 1 1410 10788 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rwidth0_
S 1414 1 3 1 0 9 1 1410 10797 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rdepth_
S 1415 1 3 2 0 9 1 1410 10771 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pr_
S 1416 14 5 0 0 9 1 1410 10809 4 1400000 A 0 0 0 0 B 0 0 0 0 0 0 0 57 4 0 0 1415 0 0 0 0 0 0 0 0 0 402 0 622 0 0 0 0 grpr
F 1416 4 1411 1412 1413 1414
A 816 2 0 0 600 9 1328 0 0 0 816 0 0 0 0 0 0 0 0 0
Z
Z
