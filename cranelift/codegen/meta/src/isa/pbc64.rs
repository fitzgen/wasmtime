use crate::cdsl::{isa::TargetIsa, settings::SettingGroupBuilder};

pub(crate) fn define() -> TargetIsa {
    let setting = SettingGroupBuilder::new("pbc64");
    TargetIsa::new("pbc64", setting.build())
}
