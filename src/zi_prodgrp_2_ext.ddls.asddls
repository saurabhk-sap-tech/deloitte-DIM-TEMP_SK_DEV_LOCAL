@AbapCatalog.sqlViewAppendName: 'ZPRPDGRP2_EXT'
@EndUserText.label: 'Production Group Extension'
extend view I_ProductGroup_2 with ZI_PRODGRP_2_EXT
association [1..1] to I_ProductGroupText_2 as _ProdGrpText on $projection.ProductGroup = _ProdGrpText.ProductGroup
                                                             and _ProdGrpText.Language = $session.system_language
{ 
  @EndUserText.label: 'Product Group Long Description'
  _ProdGrpText.ProductGroupText as ProdGrpTextLong
}
