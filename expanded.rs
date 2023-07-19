//println!("{}", no_name(1, 1));
#![feature(register_tool)]
#![register_tool(rml)]
#![feature(stmt_expr_attributes)]
#![feature(rustc_attrs)]
#![feature(unsized_fn_params)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
extern crate rml_contracts;
use rml_contracts::*;
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_947ae767_55b4_4b48_b705_f639d67bfa7b"]
fn spec_part_pre_947ae767_55b4_4b48_b705_f639d67bfa7b(a: i32, b: i32) -> bool {
    let cond: bool = !!(a + b <= i32::MAX);
    cond
}
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_c3babfb7_87d2_4f66_84ad_3e90e8bbef66"]
fn spec_part_pre_c3babfb7_87d2_4f66_84ad_3e90e8bbef66(a: i32, b: i32) -> bool {
    let cond: bool = !!(a + b >= i32::MIN);
    cond
}
#[allow(unused_variables)]
#[rml::spec::post = "spec_part_post_24ddccdc_86e9_4d3d_9c8f_58c6e5060cff"]
fn spec_part_post_24ddccdc_86e9_4d3d_9c8f_58c6e5060cff(a: i32, b: i32, result: i32) -> bool {
    let cond: bool = !!(result == a + b);
    cond
}
#[allow(unused_variables)]
#[rml::spec::div = "spec_part_div_54200f6e_3cb9_4e4d_a1d6_75cfb5731544"]
fn spec_part_div_54200f6e_3cb9_4e4d_a1d6_75cfb5731544(a: i32, b: i32) -> bool {
    let b: bool = false;
    b
}
#[allow(unused_must_use, unused_variables, dead_code)]
#[rml::spec_normal = "add_3db34ead_cbc6_4005_a989_a4ad32c53014"]
#[rml::spec_part_pre_ref = "spec_part_pre_947ae767_55b4_4b48_b705_f639d67bfa7b"]
#[rml::spec_part_pre_ref = "spec_part_pre_c3babfb7_87d2_4f66_84ad_3e90e8bbef66"]
#[rml::spec_part_post_ref = "spec_part_post_24ddccdc_86e9_4d3d_9c8f_58c6e5060cff"]
#[rml::spec_part_div_ref = "spec_part_div_54200f6e_3cb9_4e4d_a1d6_75cfb5731544"]
const add_3db34ead_cbc6_4005_a989_a4ad32c53014: bool = false;
#[rml::decl::strictly_pure]
#[rml::spec_case_ref = "add_3db34ead_cbc6_4005_a989_a4ad32c53014"]
fn add(a: i32, b: i32) -> i32 {
    a + b
}
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_ca00a2c6_6817_4b84_8c76_99e7f6cf71cb"]
fn spec_part_pre_ca00a2c6_6817_4b84_8c76_99e7f6cf71cb(a: u64, b: u64) -> bool {
    let cond: bool = !!(b != 0);
    cond
}
#[allow(unused_variables)]
#[rml::spec::post = "spec_part_post_f7b18243_35c1_44b9_b975_2842a8d4a71b"]
fn spec_part_post_f7b18243_35c1_44b9_b975_2842a8d4a71b(a: u64, b: u64, result: u64) -> bool {
    let cond: bool = !!(result == a / b);
    cond
}
#[allow(unused_variables)]
#[rml::spec::div = "spec_part_div_7075512a_8002_4a36_922e_0986494703d7"]
fn spec_part_div_7075512a_8002_4a36_922e_0986494703d7(a: u64, b: u64) -> bool {
    let b: bool = false;
    b
}
#[allow(unused_must_use, unused_variables, dead_code)]
#[rml::spec_normal = "div_f8dfe9c5_64e7_45de_9d7d_509138c2cb98"]
#[rml::spec_part_pre_ref = "spec_part_pre_ca00a2c6_6817_4b84_8c76_99e7f6cf71cb"]
#[rml::spec_part_post_ref = "spec_part_post_f7b18243_35c1_44b9_b975_2842a8d4a71b"]
#[rml::spec_part_div_ref = "spec_part_div_7075512a_8002_4a36_922e_0986494703d7"]
const div_f8dfe9c5_64e7_45de_9d7d_509138c2cb98: bool = false;
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_838476e6_718d_4137_b654_a28cd5d253c9"]
fn spec_part_pre_838476e6_718d_4137_b654_a28cd5d253c9(a: u64, b: u64) -> bool {
    let cond: bool = !!(b == 0);
    cond
}
#[allow(unused_variables)]
#[rml::spec::post = "spec_part_post_105be73f_df8c_49ab_8cd4_548f234538a1"]
fn spec_part_post_105be73f_df8c_49ab_8cd4_548f234538a1(a: u64, b: u64, result: u64) -> bool {
    let cond: bool = !!(true);
    cond
}
#[allow(unused_variables)]
#[rml::spec::div = "spec_part_div_e693689a_b357_407c_9462_d99a528cdd1f"]
fn spec_part_div_e693689a_b357_407c_9462_d99a528cdd1f(a: u64, b: u64) -> bool {
    let b: bool = false;
    b
}
#[allow(unused_must_use, unused_variables, dead_code)]
#[rml::spec_panic = "div_1fccce54_2341_4df0_96da_a5c36d211d3a"]
#[rml::spec_part_pre_ref = "spec_part_pre_838476e6_718d_4137_b654_a28cd5d253c9"]
#[rml::spec_part_post_ref = "spec_part_post_105be73f_df8c_49ab_8cd4_548f234538a1"]
#[rml::spec_part_div_ref = "spec_part_div_e693689a_b357_407c_9462_d99a528cdd1f"]
const div_1fccce54_2341_4df0_96da_a5c36d211d3a: bool = false;
#[rml::decl::pure]
#[rml::spec_case_ref = "div_1fccce54_2341_4df0_96da_a5c36d211d3a"]
#[rml::spec_case_ref = "div_f8dfe9c5_64e7_45de_9d7d_509138c2cb98"]
fn div(a: u64, b: u64) -> u64 {
    a / b
}
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_921abc3e_e57c_43ec_84dd_44943ab8ee58"]
fn spec_part_pre_921abc3e_e57c_43ec_84dd_44943ab8ee58(v: Vec<u64>) -> bool {
    let cond: bool = !!(true);
    cond
}
#[allow(unused_variables)]
#[rml::spec::post = "spec_part_post_da836839_4a3e_48b6_a4aa_55081faf69a3"]
fn spec_part_post_da836839_4a3e_48b6_a4aa_55081faf69a3(v: Vec<u64>, result: Vec<u64>) -> bool {
    let cond: bool = !!(::rml_contracts::stubs::exists(|i: usize| {
        ::rml_contracts::stubs::exists(|j: usize| result[i] == 0)
    }));
    cond
}
#[allow(unused_variables)]
#[rml::spec::div = "spec_part_div_a3d0e83a_a5fc_4d9b_9845_a9597a12a8a4"]
fn spec_part_div_a3d0e83a_a5fc_4d9b_9845_a9597a12a8a4(v: Vec<u64>) -> bool {
    let b: bool = false;
    b
}
#[allow(unused_must_use, unused_variables, dead_code)]
#[rml::spec_normal = "all_zero_566f8bd0_5225_47b1_9ab5_bb7d15f3adfc"]
#[rml::spec_part_pre_ref = "spec_part_pre_921abc3e_e57c_43ec_84dd_44943ab8ee58"]
#[rml::spec_part_post_ref = "spec_part_post_da836839_4a3e_48b6_a4aa_55081faf69a3"]
#[rml::spec_part_div_ref = "spec_part_div_a3d0e83a_a5fc_4d9b_9845_a9597a12a8a4"]
const all_zero_566f8bd0_5225_47b1_9ab5_bb7d15f3adfc: bool = false;
#[rml::spec_case_ref = "all_zero_566f8bd0_5225_47b1_9ab5_bb7d15f3adfc"]
#[allow(clippy::needless_range_loop)]
fn all_zero(mut v: Vec<u64>) -> Vec<u64> {
    for i in 0..v.len() {
        v[i] = 0;
    }
    v
}
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_f0434558_bab4_4529_b6f5_25455be4a0b3"]
fn spec_part_pre_f0434558_bab4_4529_b6f5_25455be4a0b3(a: i32, b: i32) -> bool {
    let cond: bool = !!(a + b <= i32::MAX);
    cond
}
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_85d30df7_093f_4e30_bb58_d6a373a5b3dd"]
fn spec_part_pre_85d30df7_093f_4e30_bb58_d6a373a5b3dd(a: i32, b: i32) -> bool {
    let cond: bool = !!(a + b >= i32::MIN);
    cond
}
#[allow(unused_variables)]
#[rml::spec::post = "spec_part_post_2d5ea5a9_71bc_4982_af89_d11fba8544a2"]
fn spec_part_post_2d5ea5a9_71bc_4982_af89_d11fba8544a2(a: i32, b: i32, result: i32) -> bool {
    let cond: bool = !!(result == a + b);
    cond
}
#[allow(unused_variables)]
#[rml::spec::div = "spec_part_div_0095404b_3814_4262_8166_b6fe09ec07fb"]
fn spec_part_div_0095404b_3814_4262_8166_b6fe09ec07fb(a: i32, b: i32) -> bool {
    let b: bool = false;
    b
}
#[allow(unused_must_use, unused_variables, dead_code)]
#[rml::spec_normal = "no_name_d412828b_be02_476b_b63c_27c28266562e"]
#[rml::spec_part_pre_ref = "spec_part_pre_f0434558_bab4_4529_b6f5_25455be4a0b3"]
#[rml::spec_part_pre_ref = "spec_part_pre_85d30df7_093f_4e30_bb58_d6a373a5b3dd"]
#[rml::spec_part_post_ref = "spec_part_post_2d5ea5a9_71bc_4982_af89_d11fba8544a2"]
#[rml::spec_part_div_ref = "spec_part_div_0095404b_3814_4262_8166_b6fe09ec07fb"]
const no_name_d412828b_be02_476b_b63c_27c28266562e: bool = false;
#[rml::decl::logic]
#[rml::spec_case_ref = "no_name_d412828b_be02_476b_b63c_27c28266562e"]
fn no_name(a: i32, b: i32) -> i32 {
    rec(5);
    a + b
}
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_601db3e3_2daf_4a87_a522_7d32e4fcdae6"]
fn spec_part_pre_601db3e3_2daf_4a87_a522_7d32e4fcdae6(n: u128) -> bool {
    let cond: bool = !!(n == 0);
    cond
}
#[allow(unused_variables)]
#[rml::spec::post = "spec_part_post_278a6c7f_cc6a_4cad_8aab_62a13d589d20"]
fn spec_part_post_278a6c7f_cc6a_4cad_8aab_62a13d589d20(n: u128, result: u128) -> bool {
    let cond: bool = !!(result == 0);
    cond
}
#[allow(unused_variables)]
#[rml::spec::div = "spec_part_div_14d316d2_41fb_410f_b670_18010860197a"]
fn spec_part_div_14d316d2_41fb_410f_b670_18010860197a(n: u128) -> bool {
    let b: bool = false;
    b
}
#[allow(unused_must_use, unused_variables, dead_code)]
#[rml::spec_normal = "rec_af8e65d9_3ad3_41a1_a3f7_6d3991861fca"]
#[rml::spec_part_pre_ref = "spec_part_pre_601db3e3_2daf_4a87_a522_7d32e4fcdae6"]
#[rml::spec_part_post_ref = "spec_part_post_278a6c7f_cc6a_4cad_8aab_62a13d589d20"]
#[rml::spec_part_div_ref = "spec_part_div_14d316d2_41fb_410f_b670_18010860197a"]
const rec_af8e65d9_3ad3_41a1_a3f7_6d3991861fca: bool = false;
#[allow(unused_variables)]
#[rml::spec::pre = "spec_part_pre_43716660_27f7_40de_bcc8_ef38bdce9811"]
fn spec_part_pre_43716660_27f7_40de_bcc8_ef38bdce9811(n: u128) -> bool {
    let cond: bool = !!(n > 0);
    cond
}
#[allow(unused_variables)]
#[rml::spec::post = "spec_part_post_4e49f32e_1f47_4184_be39_ee25daab68c1"]
fn spec_part_post_4e49f32e_1f47_4184_be39_ee25daab68c1(n: u128, result: u128) -> bool {
    let cond: bool = !!(result == 0);
    cond
}
#[allow(unused_variables)]
#[rml::spec::div = "spec_part_div_781fe744_9ba6_46c4_a3da_ec85139ab41a"]
fn spec_part_div_781fe744_9ba6_46c4_a3da_ec85139ab41a(n: u128) -> bool {
    let b: bool = false;
    b
}
#[allow(unused_variables)]
#[rml::spec::var = "spec_part_var_390435fe_88fe_4a04_94cb_54d3722a0cb7"]
fn spec_part_var_390435fe_88fe_4a04_94cb_54d3722a0cb7(
    n: u128,
) -> impl ::rml_contracts::WellFounded {
    n
}
#[allow(unused_must_use, unused_variables, dead_code)]
#[rml::spec_normal = "rec_f2f1ee18_1af0_45af_afce_8e61c1cc2d5f"]
#[rml::spec_part_pre_ref = "spec_part_pre_43716660_27f7_40de_bcc8_ef38bdce9811"]
#[rml::spec_part_post_ref = "spec_part_post_4e49f32e_1f47_4184_be39_ee25daab68c1"]
#[rml::spec_part_var = "spec_part_var_390435fe_88fe_4a04_94cb_54d3722a0cb7"]
#[rml::spec_part_div_ref = "spec_part_div_781fe744_9ba6_46c4_a3da_ec85139ab41a"]
const rec_f2f1ee18_1af0_45af_afce_8e61c1cc2d5f: bool = false;
#[rml::spec_case_ref = "rec_f2f1ee18_1af0_45af_afce_8e61c1cc2d5f"]
#[rml::spec_case_ref = "rec_af8e65d9_3ad3_41a1_a3f7_6d3991861fca"]
fn rec(n: u128) -> u128 {
    if n == 0 {
        0
    } else {
        rec(n - 1)
    }
}
pub fn main() {
    {
        ::std::io::_print(format_args!("RML enabled\n"));
    };
    {
        ::std::io::_print(format_args!("{0}\n", add(2, 8)));
    };
    {
        ::std::io::_print(format_args!("{0}\n", div(8, 4)));
    };
    {
        ::std::io::_print(format_args!(
            "{0:?}\n",
            all_zero(<[_]>::into_vec(
                #[rustc_box]
                ::alloc::boxed::Box::new([1, 2, 3, 4])
            ))
        ));
    };
    {
        ::std::io::_print(format_args!("{0}\n", div(4, 0)));
    };
    {
        ::std::io::_print(format_args!("{0}\n", rec(100)));
    }
}
