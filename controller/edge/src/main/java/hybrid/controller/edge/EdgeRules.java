package hybrid.controller.edge;

import com.google.common.collect.ImmutableList;

import java.util.ArrayList;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;

public class EdgeRules {
    List<EdgeRule> edgeRuleArrayList;

    public EdgeRules(List<EdgeRule> list) {
        edgeRuleArrayList = ImmutableList.copyOf(checkNotNull(list));
    }
}
