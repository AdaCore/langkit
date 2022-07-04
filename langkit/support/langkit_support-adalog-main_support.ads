--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Adalog.Generic_Main_Support;
with Langkit_Support.Images;

package Langkit_Support.Adalog.Main_Support
is new Langkit_Support.Adalog.Generic_Main_Support
  (Integer, Langkit_Support.Images.Stripped_Image);
