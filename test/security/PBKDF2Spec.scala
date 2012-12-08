/**
 * Copyright 2012 Nicolas RŽmond
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package security

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PBKDF2Spec extends Specification {

  "PBKDF2" should {

    "work with the 1st test vector" in {
      PBKDF2("password", "salt", 2, 20) must beEqualTo("ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957")
    }

    "work with the 2nd test vector" in {
      PBKDF2("password", "salt", 4096, 20) must beEqualTo("4b007901b765489abead49d926f721d065a429c1")
    }

    // It takes too long, I'm commenting it. 
    //  "work with the 3rd test vector"" should {
    //    PBKDF2("password", "salt", 16777216, 20) must beEqualTo("eefe3d61cd4da4e4e9945b3d6ba2158c2634e984")
    //  }

    "work with the 4th test vector" in {
      PBKDF2("passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 25) must beEqualTo("3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038")
    }

    "work with the 5th test vector" in {
      PBKDF2("pass\0word", "sa\0lt", 4096, 16) must beEqualTo("56fa6aa75548099dcc37d7f03425e0c3")
    }

  }
}