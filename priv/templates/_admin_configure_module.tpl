<div class="modal-body">
    <div class="form-group">
        {% wire type="click" id=#config.disruptor action={config_toggle module="mod_disrupt" key="disruptor"} %}
        <label class="control-label">
            <input type="checkbox"
                   id="{{ #config.disruptor }}" {% if m.config.mod_disrupt.disruptor.value %}checked{% endif %} />
            {_ Disruptor Enabled _}
        </label>
    </div>
</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>
