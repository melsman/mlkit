structure DriverInstance =
  Driver(
    structure TcpState = TcpState
    structure Service  = Service
  )

(* val _ = DriverInstance.run "test_1msg_inorder.txt" *)
(* val _ = DriverInstance.run "test_1msg_smallshuffle.txt" *)
(* val _ = DriverInstance.run "test_2msg_serial_inorder.txt" *)
(* val _ = DriverInstance.run "test_2msg_interleaved_permsg_inorder.txt" *)
(* val _ = DriverInstance.run "test_2msg_all_random.txt" *)
val _ = DriverInstance.run "big_test_packets.txt"
