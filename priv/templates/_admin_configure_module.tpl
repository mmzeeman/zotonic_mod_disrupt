<div class="modal-body">
    <div class="form-group">
        {% wire type="click" id=#config.disruptor action={config_toggle module="mod_disrupt" key="disruptor"} %}
        <label class="control-label">
            <input type="checkbox"
                   id="{{ #config.disruptor }}" {% if m.config.mod_disrupt.disruptor.value %}checked{% endif %} />
            {_ Run Disruptor (Havoc) _}
        </label>
    </div>

    <div class="form-group">
        {% wire type="click" id=#config.dispatch_disruption action={config_toggle module="mod_disrupt" key="dispatch_disruption"} %}
        <label class="control-label">
            <input type="checkbox"
                   id="{{ #config.dispatch_disruption }}" {% if m.config.mod_disrupt.dispatch_disruption.value %}checked{% endif %} />
            {_ Base Dispatch Disruption Enabled _}
        </label>
    </div>

</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>

