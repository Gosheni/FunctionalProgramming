Caml1999N033����            .src/monads.mli����   C    9  N�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����&monads��.<command-line>A@A�A@G@@��A@@�A@H@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������$Core��.src/monads.mliA@E�A@I@A��A@@�A@I@@��A@@�A@I@������)State_int��l���l��@�����������%Monad!S��!n���"n��@��$n���%n��@@��'n���(n��@@��*n���+n��@���Р#run��3p���4p��@��@����!t��=p���>p��@���!a��Dp���Ep��@@@@��Gp���Hp��@@@�����!a��Pp���Qp��@@@�����#int��Yp���Zp��@@��\p���]p��@@@@��_p���`p��@@@��bp���cp��@@@@���)ocaml.doc��@@ ��@@ �A�������	R [run x] is the underlying data and the final state of the given monad value [x]. ��tq���uq�M@@��wq���xq�M@@@@��zq���{q�M@@��}q���~q�M@@���p����p��@���p����p��@���Р#set���sOW��sOZ@��@����#int���sO]��sO`@@���sO]��sO`@@@����!t���sOi��sOj@�����$unit���sOd��sOh@@���sOd��sOh@@@@���sOd��sOj@@@���sO]��sOj@@@@���Q��g@@ ��h@@ �A�������	4 [set i] is a monadic value with the new state [i]. ���tko��tk�@@���tko��tk�@@@@���tko��tk�@@���tko��tk�@@���sOS��sOj@���sOS��sOj@���Р#get���v����v��@����!t���v����v��@�����#int���v����v��@@���v����v��@@@@���v����v��@@@@�������@@ ���@@ �A�������	Z [get] is a monadic value whose underlying data is the state, and the state is unchanged. ��w���w�!@@��w���w�!@@@@��
w���w�!@@��w���w�!@@��v���v��@��v���v��@���Р#inc��y#+�y#.@����!t��$y#5�%y#6@�����#int��-y#1�.y#4@@��0y#1�1y#4@@@@��3y#1�4y#6@@@@���Ѱ��@@ ���@@ �A�������	` [inc] is a monadic value whose underlying data is the state + 1, and the state is incremented. ��Dz7;�Ez7�@@��Gz7;�Hz7�@@@@��Jz7;�Kz7�@@��Mz7;�Nz7�@@��Py#'�Qy#6@��Sy#'�Ty#6@@��Vm���W{��@@@��Yl���Z{��@��\l���]{��@������+Stack_monad��f L)0�g L);@�����������%Monad"S2��v U���w U��@��y U���z U��@@��| U���} U��@@�� U���� U��@���Р#run��� W���� W��@��@����!t��� W���� W��@���!a��� W���� W��@@@���!e��� W���� W��@@@@��� W���� W��@@@��!a��� W���� W�@@@��� W���� W�@@@@���J��`@@ ��a@@ �A�������
  : [run x] takes [x] out of monad land by providing an initial empty state and throwing away the final state.
        Recall that 'a is the underlying data type, and 'e is the type of value in the stack. From this signature,
        we see that the data in the stack is thrown away, and the underlying data is kept. ��� X�� Z�E@@��� X�� Z�E@@@@��� X�� Z�E@@��� X�� Z�E@@��� W���� W�@��� W���� W�@���Р$push��� \GO�� \GS@��@��!e��� \GV�� \GX@@@����!t��� \Gg�� \Gh@�����$unit��� \G]�� \Ga@@��� \G]�� \Ga@@@���!e��� \Gc�� \Ge@@@@��� \G\�� \Gh@@@��� \GV�� \Gh@@@@�������@@ ���@@ �A�������	C [push x] is a monadic value that has [x] on the top of the stack. �� ]im� ]i�@@�� ]im� ]i�@@@@�� ]im� ]i�@@�� ]im� ]i�@@�� \GK� \Gh@�� \GK� \Gh@���Р#pop��' _���( _��@����!t��/ _���0 _��@���!e��6 _���7 _��@@@���!e��= _���> _��@@@@��@ _���A _��@@@@���ް��@@ ���@@ �A�������	� [pop] is a monadic value whose underlying data is the top value of the stack, and the new state
        no longer has the top value. This may throw an exception when run. ��Q `���R a8�@@��T `���U a8�@@@@��W `���X a8�@@��Z `���[ a8�@@��] _���^ _��@��` _���a _��@���Р(is_empty��i c���j c��@����!t��q c���r c��@�����$bool��z c���{ c��@@��} c���~ c��@@@���!e��� c���� c��@@@@��� c���� c��@@@@���%��;@@ ��<@@ �A�������	_ [is_empty] is a monadic value whose underlying data is true if and only if the stack is empty ��� d���� d�@@��� d���� d�@@@@��� d���� d�@@��� d���� d�@@��� c���� c��@��� c���� c��@@��� M>@�� e@@@��� L))�� e@��� L))�� e@���Р4are_balanced_mutable��� s���� s��@��@����&string��� s���� s��@@��� s���� s��@@@����$bool��� s���� s��@@��� s���� s��@@@��� s���� s��@@@@���r���@@ ���@@ �A�������	� [are_balanced_mutable s] is true if and only if the string [s] is a string of opening '(' and
    closing ')' parentheses that are balanced.

    See `monads.ml` for the implementation. ��� t���� wIw@@��� t���� wIw@@@@��� t���� wIw@@��� t���� wIw@@��� s���� s��@��� s���� s��@���Р4are_balanced_monadic��� yy}�� yy�@��@����&string�� yy�� yy�@@��
 yy�� yy�@@@����$bool�� yy�� yy�@@�� yy�� yy�@@@�� yy�� yy�@@@@�������@@ ���@@ �A�������	� [are_balanced_monadic s] is the same as [are_balanced_mutable s] but uses no mutation.
    
    See `monads.ml` for some helper functions, and complete them with your implementation. ��) z���* |`@@��, z���- |`@@@@��/ z���0 |`@@��2 z���3 |`@@��5 yyy�6 yy�@��8 yyy�9 yy�@������/Exception_stack��B �z��C �z�@�����������%Monad"S2��R ����S ���@��U ����V ���@@��X ����Y ���@@��[ ����\ ���@���Р#run��d ����e ���@��@����!t��n ����o ���@���!a��u ����v ���@@@���!e��| ����} ���@@@@�� ����� ���@@@����&result��� ����� ���@���!a��� ����� ���@@@�����#exn��� ����� ���@@��� ����� ���@@@@��� ����� ���@@@��� ����� ���@@@@@��� ����� ���@��� ����� ���@���Р$push��� ����� ���@��@��!e��� ����� ���@@@����!t��� ����� ���@�����$unit��� ����� ���@@��� ����� ���@@@���!e��� ����� ���@@@@��� ����� ���@@@��� ����� ���@@@@@��� ����� ���@��� ����� ���@���Р#pop��� ���� ��@����!t��� ���� ��@���!e��� ���� ��@@@���!e��� ���� ��@@@@��  ��� ��@@@@@�� ��� ��@�� ��� ��@���Р(is_empty�� �� �&@����!t�� �4� �5@�����$bool��  �*�! �.@@��# �*�$ �.@@@���!e��* �0�+ �2@@@@��- �)�. �5@@@@@��0 ��1 �5@��3 ��4 �5@@��6 ����7 �6;@@@��9 �zz�: �6;@��< �zz�= �6;@���Р9are_balanced_more_monadic��E �=A�F �=Z@��@����&string��O �=]�P �=c@@��R �=]�S �=c@@@����$bool��Z �=g�[ �=k@@��] �=g�^ �=k@@@��` �=]�a �=k@@@@@��c �==�d �=k@��f �==�g �=k@@